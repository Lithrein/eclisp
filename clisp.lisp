(in-package #:clisp)

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

(defun compile-cpp-include (include-forms to-stream)
  "Compile an include directive: (%include header-list)
where header-list is a list of header names either enclosed in double-quotes
or in angle-brackets."
  (loop for filename in include-forms do
    (format to-stream
	    (cond ((symbolp filename) "#include ~a~%")
		  (t "#include \"~a\"~%"))
	    filename)))

(defun compile-cpp-define (form to-stream)
  "Compile a define directive: %(define name substitution) and write it
to TO-STREAM. FORM is a cons made of NAME and SUBSTITUTION.
NAME and SUBSTITUTION are both strings and should enclosed in double-quotes.
NAME can contain parenthesis just like a C macro does, i.e., no spaces before
the parenthesis.  SUBSTITUTION should be valid C code."
  (format to-stream "#define ~{~a~^ ~}~%" form))

(defun compile-arith-binop (form to-stream &optional (cpp nil))
  "Compile a FORM beginning with an arithmetic operator (+ - * / % ^ | || & &&
~ << >>) into a valid C expression and write it on TO-STREAM.
The CPP parameter enable C Preprocessor mode, restricting what can appear in
the expression."
  (let ((compile-proxy (if cpp #'compile-cpp-cond-expr #'compile-form)))
    (destructuring-bind (op &rest args) form
      (format to-stream "(")
      (do ((cur args (cdr cur)))
          ((not cur))
        (funcall compile-proxy (car cur) to-stream)
        (when (cdr cur)
          (format to-stream " ~a " op)))
      (format to-stream ")"))))

(defun compile-cmp-op (form to-stream &optional (cpp nil))
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
        (funcall compile-proxy (car cur) to-stream)
        (format to-stream " ~a " op)
        (funcall compile-proxy (cadr cur) to-stream)
        (format to-stream ")")
        (when (cddr cur)
          (format to-stream " && ")))
      (format to-stream ")"))))

(defun compile-cpp-cond-expr (form to-stream)
  "Compile a preprocessor condition FORM, i.e.,  one that may appear after
#if and #elif, and write it on TO-STREAM."
  (if (consp form)
      (destructuring-bind (op &rest args) form
        (cond
          ((string= "defined" op)
           (format to-stream "defined~a" args))
          ((member (string op) '("<" ">" "<=" ">=" ">" "==") :test #'equal)
           (compile-cmp-op form to-stream t))
          ((member (string op) '("+" "-" "*" "/" "%" "^" "|" "||" "&" "&&" "~" "<<" ">>")
                   :test #'equal)
           (compile-arith-binop form to-stream t))))
      (format to-stream "~a" form)))

(defun compile-cpp-if (form to-stream)
  "Compile a if preprocessor directive FORM and write it on TO-STREAM"
  (do ((cur form (cdr cur))
       (first t nil))
      ((not cur))
    (destructuring-bind (cond &rest body) (car cur)
      (if first
          (format to-stream "#if ")
          (format to-stream (if (and (symbolp cond) (string= cond "t")) "#else" "#elif ")))
      (unless (and (symbolp cond) (string= cond "t")) (compile-cpp-cond-expr cond to-stream))
      (format to-stream "~%")
      (loop for b in body do
            (compile-form b to-stream))))
  (format to-stream "#endif~%"))

(defun compile-type (type to-stream)
(defun print-ll (l to-stream)
  "Flatten and print to TO-STREAM the list produced by PRINT-C-TYPE."
  (cond ((null l) nil)
        ((consp l)
         (cond ((listp (car l)) (print-ll (car l) to-stream))
               (t (let ((*print-case* :downcase))
                    (format to-stream "~a" (car l)))))
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
                                collect (list "," (print-c-type "" tt nil))))
                         ")")))
    ((string= "volatile" (car type))
     (print-c-type "" (cadr type)  (list "volatile " acc name)))
    ((string= "const" (car type))
     (print-c-type "" (cadr type)  (list "const " acc name)))
    (t (list (car type) (if (null acc) nil " ") acc (if (string= name "") nil " ") name))))

(defun compile-type (name type to-stream)
  "Compile TYPE and write it on TO-STREAM."
  (print-ll (print-c-type name type nil) to-stream))

(defun compile-defvar (form to-stream)
  "Compile a form which declares a global variable.
                  (defvar (var type) value documentation)
VALUE is optional, TYE is optional as well and defaults to int.  If TYPE is
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
      (format to-stream "/* ~a  */~%"
              (regex-replace-all "\\n" documentation "
   ")))
    (if (null type)
        (format to-stream "int")
        (compile-type type to-stream))
    (format to-stream " ~a" var)
    (when value (format to-stream " = ~a" value))
    (format to-stream ";~%")))

(defun compile-form (form to-stream)
  "Compile a clisp FROM and write it on TO-STREAM"
  (if (consp form)
      (destructuring-bind (op &rest args) form
        (cond
          ((string= "%include" (string op)) (compile-cpp-include args to-stream))
          ((string= "%define"  (string op)) (compile-cpp-define args to-stream))
          ((string= "%if"      (string op)) (compile-cpp-if args to-stream))
          ((string= "defvar"   (string op)) (compile-defvar args to-stream))
          ((member (string op) '("<" ">" "<=" ">=" ">" "=" "&&" "||") :test #'equal)
           (compile-cmp-op form to-stream))
          ((member (string op) '("+" "-" "*" "/" "%" "^" "|" "&" "~" "<<" ">>")
                   :test #'equal)
           (compile-arith-binop form to-stream))
          (t (format t "unknown construct ~a" (car form)))))
      (format to-stream "~a" form)))

(defun compile-clisp (from-stream to-stream)
  "Write the result of the compilation of the content of FROM-STREAM into
TO-STREAM."
  (loop for form = (with-custom-reader "'|" (read from-stream nil))
        while form do (compile-form form to-stream)))

(defun main ()
  "The entry point."
  (compile-clisp *standard-input* *standard-output*))
