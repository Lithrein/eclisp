(in-package #:eclisp)

(defmacro with-custom-reader (ignore-chars &body body)
  "Enable case sensitivity and disable the reader macros associated to
characters in the IGNORE-CHARS string while performing READ."
  `(let ((*readtable* (copy-readtable)))
     (loop for cc across ,ignore-chars do
       (set-macro-character
        cc
        (lambda (stream char)
          (concatenate
           'string
           (make-string 1 :initial-element char)
           (loop for c = (peek-char nil stream nil nil)
                 while (and c (eql c (peek-char t stream nil nil)))
                 collect (read-char stream) into letters
                 finally (return (coerce letters 'string)))))
        t *readtable*))
     (setf (readtable-case *readtable*) :preserve)
     (progn
       ,@body)))

(defun compile-cpp-include (include-forms indent to-stream)
  "Compile an include directive: (%include header-list)
where header-list is a list of header names either enclosed in double-quotes
or in angle-brackets."
  (loop for filename in include-forms do
    (format to-stream "~v@{~C~:*~}" indent #\Space)
    (format to-stream
	    (cond ((symbolp filename) "#include ~a~%")
		  (t "#include \"~a\"~%"))
	    filename)))

(defun compile-cpp-define (form indent to-stream)
  "Compile a define directive: %(define name substitution) and write it
to TO-STREAM. FORM is a cons made of NAME and SUBSTITUTION.
NAME and SUBSTITUTION are both strings and should enclosed in double-quotes.
NAME can contain parenthesis just like a C macro does, i.e., no spaces before
the parenthesis.  SUBSTITUTION should be valid C code."
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "#define ~{~a~^ ~}~%" form))

(defun compile-arith-binop (form indent to-stream &optional (cpp nil))
  "Compile a FORM beginning with an arithmetic operator (+ - * / % ^ | || & &&
~ << >>) into a valid C expression and write it on TO-STREAM.
The CPP parameter enable C Preprocessor mode, restricting what can appear in
the expression."
  (let ((compile-proxy (if cpp #'compile-cpp-cond-expr #'compile-form)))
    (destructuring-bind (op &rest args) form
      (format to-stream "~v@{~C~:*~}" indent #\Space)
      (format to-stream "(")
      (do ((cur args (cdr cur)))
          ((not cur))
        (funcall compile-proxy (car cur) nil 0 to-stream)
        (when (cdr cur)
          (format to-stream " ~a " op)))
      (format to-stream ")"))))

(defun compile-cmp-op (form indent to-stream &optional (cpp nil))
  "Compile a FORM beginning with a compare operator (< > <= >= > ==) into a valid
C expression and write it on TO-STREAM.
The CPP parameter enable C Preprocessor mode, restricting what can appear in
the expression."
  (let ((compile-proxy (if cpp #'compile-cpp-cond-expr #'compile-form)))
    (destructuring-bind (op &rest args) form
      (format to-stream "(")
      (do ((cur args (cdr cur)))
          ((not (cdr cur)))
        (format to-stream "(")
        (funcall compile-proxy (car cur) nil indent to-stream)
        (format to-stream " ~a " op)
        (funcall compile-proxy (cadr cur) nil indent to-stream)
        (format to-stream ")")
        (when (cddr cur)
          (format to-stream " && ")))
      (format to-stream ")"))))

(defun compile-cpp-cond-expr (form stmtp indent to-stream)
  "Compile a preprocessor condition FORM, i.e.,  one that may appear after
#if and #elif, and write it on TO-STREAM."
  (declare (ignore indent))
  (declare (ignore stmtp))
  (if (consp form)
      (destructuring-bind (op &rest args) form
        (cond
          ((string= "defined" op)
           (format to-stream "defined~a" args))
          ((member (string op) '("<" ">" "<=" ">=" ">" "==") :test #'equal)
           (compile-cmp-op form indent to-stream t))
          ((member (string op) '("+" "-" "*" "/" "%" "^" "|" "||" "&" "&&" "~" "<<" ">>")
                   :test #'equal)
           (compile-arith-binop form 0 to-stream t))))
      (format to-stream "~a" form)))

(defun compile-cond-expr (form indent to-stream)
  "Compile a condition FORM, i.e.,  one that may appear after
and if, and write it on TO-STREAM."
  (declare (ignore indent))
  (if (consp form)
      (destructuring-bind (op &rest args) form
        (cond
          ((member (string op) '("<" ">" "<=" ">=" ">" "==" "!=") :test #'equal)
           (compile-cmp-op form indent to-stream))
          ((member (string op) '("+" "-" "*" "/" "%" "^" "|" "||" "&" "&&" "~" "<<" ">>")
                   :test #'equal)
           (compile-arith-binop form 0 to-stream))))
      (format to-stream "~a" form)))

(defun compile-cpp-if (form indent to-stream)
  "Compile a if preprocessor directive FORM and write it on TO-STREAM"
  (do ((cur form (cdr cur))
       (first t nil))
      ((not cur))
    (destructuring-bind (cond &rest body) (car cur)
      (format to-stream "~v@{~C~:*~}" indent #\Space)
      (if first
          (format to-stream "#if ")
          (format to-stream (if (and (symbolp cond) (string= cond "t"))
                                "~%~v@{~C~:*~}#else" "~%~v@{~C~:*~}#elif ")
                  indent #\Space))
      (unless (and (symbolp cond) (string= cond "t")) (compile-cpp-cond-expr cond nil 0 to-stream))
      (format to-stream "~%")
      (loop for b in body do
        (compile-form b nil (+ 2 indent) to-stream))))
  (format to-stream "~%~v@{~C~:*~}#endif~%" indent #\Space))

(defun print-ll (l to-stream)
  "Flatten and print to TO-STREAM the list produced by PRINT-C-TYPE."
  (cond ((null l) nil)
        ((consp l)
         (cond ((listp (car l)) (print-ll (car l) to-stream))
               (t (format to-stream "~a" (car l))))
         (print-ll (cdr l) to-stream))))

(defun print-c-type (name type acc)
  "Create a nested list which represents the variable NAME of TYPE.
ACC should be NIL at first."
  (cond
    ((string= "ptr"  (car type))
     (print-c-type "" (cadr type)  (list "(*" acc name ")")))
    ((string= "array" (car type))
     (print-c-type "" (caddr type)
                   (if (and (null acc) (string= name ""))
                       (list "[" (cadr type) "]")
                       (list "(" acc name ")[" (cadr type) "]"))))
    ((string= "fun" (car type))
     (print-c-type "" (cadadr type)
                   (list "(" acc name ")("
                         ((lambda (l) (cons (cdar l) (cdr l)))
                          (loop for tt in (cddr type)
                                collect (list ", "
                                              (if (symbolp (car tt))
                                                  (print-c-type (car tt) (cadr tt) nil)
                                                (print-c-type "" (car tt) nil)))))
                         ")")))
    ((string= "volatile" (car type))
     (print-c-type "" (cadr type)  (list "volatile " acc name)))
    ((string= "const" (car type))
     (print-c-type "" (cadr type)  (list "const " acc name)))
    (t (list (car type) (if (null acc) nil " ") acc (if (string= name "") nil " ") name))))

(defun compile-type (name type to-stream)
  "Compile TYPE and write it on TO-STREAM."
  (print-ll (print-c-type name type nil) to-stream))

(defun compile-progn (form indent to-stream)
  (format to-stream "~v@{~C~:*~}{~%" indent #\Space)
  (loop for f in form do (compile-form f t (+ 2 indent) to-stream))
  (format to-stream "~v@{~C~:*~}}~%" indent #\Space))

(defun compile-defvar (form stmtp indent to-stream)
  "Compile a form which declares a global variable.
                  (defvar (var type) value documentation)
VALUE is optional, TYPE is optional as well and defaults to int.  If TYPE is
not present the parenthesis around (VAR TYPE) are optional. DOCUMENTATION is
also optional, however, a default value is mandatory before the documentation
string."
  (let ((var nil) (type nil) (value nil) (documentation nil))
    (if (listp (car form))
        (progn
          (setf var (caar form))
          (setf type (cadar form)))
        (setf var (car form)))
    (unless (null (cdr form)) (setf value (cadr form)))
    (unless (null (cddr form)) (setf documentation (caddr form)))
    (unless (null documentation)
      (progn
        (format to-stream "~v@{~C~:*~}" indent #\Space)
        (format to-stream "/* ~a  */~%"
                (regex-replace-all "\\n" documentation
                                   (concatenate 'string '(#\Newline) "   "
                                                (format nil "~v@{~C~:*~}" indent #\Space))))))
    (format to-stream "~v@{~C~:*~}" indent #\Space)
    (compile-type var (if (null type) '(|int|) type) to-stream)
    (when value (format to-stream " = ~a"
                        (with-output-to-string (s) (compile-form value nil 0 s))))
    (when stmtp (format to-stream ";~%"))))

(defun compile-defun (form indent to-stream)
  "Compile a form which declares a global variable.
                  (defun (f type) documentation body)
BODY is optional, TYPE is optional as well and defaults to void.  If TYPE is
not present the parenthesis around (VAR TYPE) are optional. DOCUMENTATION is
also optional"
  (let ((var nil) (type nil) (body nil) (documentation nil))
    (if (listp (car form))
        (progn
          (setf var (caar form))
          (setf type (cadar form)))
        (setf var (car form)))
    (unless (null (cdr form)) (setf documentation (cadr form)))
    (unless (null (cddr form)) (setf body (cddr form)))
    (unless (null documentation)
      (progn
        (format to-stream "~v@{~C~:*~}" indent #\Space)
        (format to-stream "/* ~a  */~%"
                (regex-replace-all "\\n" documentation
                                   (concatenate 'string '(#\Newline) "   "
                                                (format nil "~v@{~C~:*~}" indent #\Space))))))
    (format to-stream "~v@{~C~:*~}" indent #\Space)
    (compile-type var (if (null type) '(void) type) to-stream)
    (when body
      (format to-stream "~%")
      (compile-progn body indent to-stream))))

(defun compile-if (form indent to-stream)
  (format to-stream "~v@{~C~:*~}if (" indent #\Space)
  (compile-form (car form) nil 0 to-stream)
  (format to-stream ")~%")
  (compile-form (cadr form) t (+ 2 indent) to-stream)
  (unless (null (caddr form))
    (format to-stream "~v@{~C~:*~}else ~%" indent #\Space)
    (compile-form (caddr form) t (+ 2 indent) to-stream)))

(defun compile-set (form stmtp indent to-stream)
  (if stmtp
      (format to-stream "~v@{~C~:*~}" indent #\Space)
      (format to-stream "("))
  (compile-form (car form) nil 0 to-stream)
  (format to-stream " = ")
  (compile-form (cadr form) nil 0 to-stream)
  (if stmtp
      (format to-stream ";~%")
      (format to-stream ")")))

(defun compile-dot (form indent to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (compile-form (car form) nil indent to-stream)
  (format to-stream ".")
  (compile-form (cadr form) nil indent to-stream))

(defun compile-arrow (form indent to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (compile-form (car form) nil indent to-stream)
  (format to-stream "->")
  (compile-form (cadr form) nil indent to-stream))

(defun compile-aref (form indent to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (compile-form (car form) nil indent to-stream)
  (format to-stream "[")
  (compile-form (cadr form) nil indent to-stream)
  (format to-stream "]"))

(defun compile-addr (form indent to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "&")
  (compile-form (car form) nil indent to-stream))

(defun compile-comment (form indent to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "//~a~%" (car form)))

(defun compile-form (form stmtp indent to-stream)
  "Compile an eclisp FROM and write it on TO-STREAM"
  (if (consp form)
      (destructuring-bind (op &rest args) form
        (cond
          ((string= "%include" (string op)) (compile-cpp-include args indent to-stream))
          ((string= "%define"  (string op)) (compile-cpp-define args indent to-stream))
          ((string= "%if"      (string op)) (compile-cpp-if args indent to-stream))
          ((string= "%comment" (string op)) (compile-comment args indent to-stream))
          ((string= "defvar"   (string op)) (compile-defvar args stmtp indent to-stream))
          ((string= "defun"    (string op)) (compile-defun args indent to-stream))
          ((string= "progn"    (string op)) (compile-progn args indent to-stream))
          ((string= "set"      (string op)) (compile-set args stmtp indent to-stream))
          ((string= "addr"     (string op)) (compile-addr args indent to-stream))
          ((string= "."        (string op)) (compile-dot args indent to-stream))
          ((string= "->"       (string op)) (compile-arrow args indent to-stream))
          ((string= "aref"     (string op)) (compile-aref args indent to-stream))
          ((string= "if"       (string op)) (compile-if args indent to-stream))
          ((member (string op) '("<" ">" "<=" ">=" ">" "==" "!=" "&&" "||") :test #'equal)
           (compile-cmp-op form indent to-stream))
          ((member (string op) '("+" "-" "*" "/" "%" "^" "|" "&" "~" "<<" ">>")
                   :test #'equal)
           (compile-arith-binop form indent to-stream)
           (when stmtp (format to-stream ";~%")))
          (t
           (format to-stream "~v@{~C~:*~}" indent #\Space)
           (format to-stream "~a (~{~a~^, ~})"
                   (car form)
                   (mapcar (lambda (x) (if (stringp x) (format nil "\"~a\"" x) x))
                           (cdr form)))
           (when stmtp (format to-stream ";~%")))))
    (progn
      (format to-stream "~v@{~C~:*~}" indent #\Space)
      (format to-stream "~a" form))))

(defun compile-eclisp (from-stream to-stream)
  "Write the result of the compilation of the content of FROM-STREAM into
TO-STREAM."
  (loop for form = (parse from-stream)
        while form do (compile-form form t 0 to-stream)))

(defun main ()
  "The entry point."
  (compile-eclisp *standard-input* *standard-output*))
