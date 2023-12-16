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
    ((not (consp type))
     (list type (if (null acc) nil " ") acc (if (string= name "") nil " ") name))
    ((string= "ptr"  (car type))
     (print-c-type ""
                   (if (consp (cadr type)) (cadr type) (cdr type))
                   (list "(*" acc name ")")))
    ((string= "array" (car type))
     (print-c-type ""
                   (if (consp (caddr type)) (caddr type) (cddr type))
                   (if (and (null acc) (string= name ""))
                       (list "[" (cadr type) "]")
                       (list "(" acc name ")[" (cadr type) "]"))))
    ((string= "fun" (car type))
     (print-c-type ""
                   (if (consp (cdadr type)) (cadadr type) (cdadr type))
                   (list "(" acc name ")("
                         ((lambda (l) (cons (cdar l) (cdr l)))
                          (loop for tt in (cddr type)
                                collect (list ", "
                                              (if (symbolp (car tt))
                                                  (print-c-type (car tt) (cadr tt) nil)
                                                  (print-c-type "" (car tt) nil)))))
                         ")")))
    ((string= "enum" (car type))
     (print-c-type name nil
                   (list "enum "
                         (if (consp (cadr type))
                             (list #\Newline "{"
                                   ((lambda (l) (cons (cdar l) (cdr l)))
                                    (loop for tt in (cadr type)
                                          collect (list "," #\Newline
                                                        (if (consp tt)
                                                            (list "  " (car tt) " = " (cadr tt))
                                                          (list "  " tt)))))
                                   #\Newline "}")
                           (list (cadr type) #\Newline "{"
                                 ((lambda (l) (cons (cdar l) (cdr l)))
                                  (loop for tt in (cddr type)
                                        collect (list "," #\Newline
                                                      (if (consp tt)
                                                          (list "  " (car tt) " = " (cadr tt))
                                                        (list "  " tt)))))
                                 #\Newline "}")))))
    ((string= "struct" (car type))
     (list "struct "
           (print-c-type name nil
                         (if (consp (cadr type))
                             (list "{" #\Newline
                                   (loop for tt in (cdr type)
                                         collect (list "  "
                                                       (if (symbolp (car tt))
                                                           (print-c-type (car tt) (cadr tt) nil)
                                                         (print-c-type "" (car tt) nil)) ";" #\Newline))
                                   "}")
                           (list (cadr type)
                                 (when (consp (caddr type))
                                   (list " {" #\Newline
                                         (loop for tt in (cddr type)
                                               collect (list "  "
                                                             (if (symbolp (car tt))
                                                                 (print-c-type (car tt) (cadr tt) nil)
                                                               (print-c-type "" (car tt) nil)) ";" #\Newline))
                                         "}")))))))
    ((string= "union" (car type))
     (list "union "
           (print-c-type name nil
                         (if (consp (cadr type))
                             (list "{" #\Newline
                                   (loop for tt in (cddr type)
                                         collect (list "  "
                                                       (if (symbolp (car tt))
                                                           (print-c-type (car tt) (cadr tt) nil)
                                                         (print-c-type "" (car tt) nil)) ";" #\Newline))
                                   "}")
                           (list (cadr type)
                                 (when (consp (caddr type))
                                   (list " {" #\Newline
                                         (loop for tt in (cdddr type)
                                               collect (list "  "
                                                             (if (symbolp (car tt))
                                                                 (print-c-type (car tt) (cadr tt) nil)
                                                               (print-c-type "" (car tt) nil)) ";" #\Newline))
                                         "}")))))))
    ((string= "volatile" (car type))
     (print-c-type ""
                   (if (consp (cadr type)) (cadr type) (cdr type))
                   (list "volatile " acc name)))
    ((string= "typedef" (car type))
     (list "typedef " (print-c-type name
                                    (if (consp (cadr type)) (cadr type) (cdr type))
                                    acc)))
    ((string= "static" (car type))
     (list "static " (print-c-type name
                                   (if (consp (cadr type)) (cadr type) (cdr type))
                                   name)))
    ((string= "extern" (car type))
     (list "extern " (print-c-type name
                                   (if (consp (cadr type)) (cadr type) (cdr type))
                                   name)))
    ((string= "long" (car type))
     (list "long " (print-c-type name
                                 (if (consp (cadr type)) (cadr type) (cdr type))
                                 nil)))
    ((string= "short" (car type))
     (list "short " (print-c-type name
                                  (if (consp (cadr type)) (cadr type) (cdr type))
                                  nil)))
    ((string= "const" (car type))
     (print-c-type ""
                   (if (consp (cadr type)) (cadr type) (cdr type))
                   (list "const " acc name)))
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
                  (def var type value documentation)"
  (let ((var nil) (type nil) (value nil) (documentation nil))
    (if (consp (car form))
        ;; no name, hence no value
        (progn
          (setf type (car form))
          (setf documentation (cadr form)))
      ;; name is present
      (progn
        (setf var (car form))
        (if (consp (cadr form))
            (progn
              (setf type (cadr form))
              (setf value (caddr form))
              (setf documentation (cadddr form)))
          (progn
            (setf value (cadr form))
            (setf documentation (caddr form))))))
    (when (and (symbolp value) (string= (string value) "%nothing")) (setf value nil))
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
                  (def f (-> rettype params) documentation body)
BODY is optional. DOCUMENTATION is optional"
  (let ((var nil) (type nil) (body nil) (documentation nil))
    (setf var (car form))
    (setf type (cadr form))
    (if (stringp (caddr form))
        (progn
          (setf documentation (caddr form))
          (setf body (cdddr form)))
      (setf body (cddr form)))
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
      (compile-progn body indent to-stream))
    (when (not body)
      (format to-stream ";~%"))))

(defun compile-def (form stmtp indent to-stream)
  (let ((category nil) (var nil) (type nil))
    (if (consp (car form))
        (setf type (car form))
      ;; name is present
      (progn
        (setf var (car form))
        (when (consp (cadr form)) (setf type (cadr form)))))
    (cond ((and type (string= (car type) "fun"))
           (compile-defun form indent to-stream))
          (t (compile-defvar form stmtp indent to-stream)))))

(defun compile-if (form indent to-stream)
  (format to-stream "~v@{~C~:*~}if (" indent #\Space)
  (compile-form (car form) nil 0 to-stream)
  (format to-stream ")~%")
  (compile-form (cadr form) t (+ 2 indent) to-stream)
  (unless (null (caddr form))
    (format to-stream "~v@{~C~:*~}else ~%" indent #\Space)
    (compile-form (caddr form) t (+ 2 indent) to-stream)))

(defun compile-set (form stmtp op indent to-stream)
  (if stmtp
      (format to-stream "~v@{~C~:*~}" indent #\Space)
      (format to-stream "("))
  (compile-form (car form) nil 0 to-stream)
  (format to-stream " ~a " op)
  (compile-form (cadr form) nil 0 to-stream)
  (if stmtp
      (format to-stream ";~%")
      (format to-stream ")")))

(defun compile-seq (form stmtp indent to-stream)
  (if stmtp
      (format to-stream "~v@{~C~:*~}" indent #\Space))
  (let ((cur form))
    (loop while cur do
          (compile-form (car cur) nil 0 to-stream)
          (if (cdr cur) (format to-stream ", "))
          (setf cur (cdr cur))))
  (if stmtp
      (format to-stream ";~%")))

(defun compile-dot (form stmtp indent to-stream)
  (if stmtp
      (format to-stream "~v@{~C~:*~}" indent #\Space))
  (let ((cur form))
    (loop while cur do
          (compile-form (car cur) nil 0 to-stream)
          (if (cdr cur) (format to-stream "."))
          (setf cur (cdr cur))))
  (if stmtp
      (format to-stream ";~%")))

(defun compile-arrow (form stmtp indent to-stream)
  (if stmtp
      (format to-stream "~v@{~C~:*~}" indent #\Space))
  (let ((cur form))
    (loop while cur do
          (compile-form (car cur) nil 0 to-stream)
          (if (cdr cur) (format to-stream "->"))
          (setf cur (cdr cur))))
  (if stmtp
      (format to-stream ";~%")))

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

(defun compile-deref (form indent to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "*")
  (compile-form (car form) nil indent to-stream))

(defun compile-while (form indent to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "while (")
  (compile-form (car form) nil 0 to-stream)
  (format to-stream ")~%")
  (compile-form (cadr form) t (+ 2 indent) to-stream))

(defun compile-do-while (form indent to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "do")
  (format to-stream "~%")
  (compile-form (cadr form) t (+ 2 indent) to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "while ")
  (compile-cond-expr (car form) 0 to-stream)
  (format to-stream ";~%"))

(defun compile-ite (form stmtp indent to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (compile-cond-expr (car form) 0 to-stream)
  (format to-stream " ? ")
  (compile-form (cadr form) nil 0 to-stream)
  (format to-stream " : ")
  (compile-form (caddr form) nil 0 to-stream)
  (when stmtp (format to-stream ";~%")))

(defun compile-for (form indent to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "for (")
  (compile-form (car form) nil 0 to-stream)
  (format to-stream "; ")
  (compile-cond-expr (cadr form) 0 to-stream)
  (format to-stream "; ")
  (compile-form (caddr form) nil 0 to-stream)
  (format to-stream ")~%")
  (compile-form (cadddr form) t (+ 2 indent) to-stream))

(defun compile-return (form indent to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "return ")
  (compile-form (car form) nil 0 to-stream)
  (format to-stream ";~%"))

(defun compile-break (form indent to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "break;~%"))

(defun compile-continue (form indent to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "continue;~%"))

(defun compile-cast (form stmtp indent to-stream)
  (if stmtp
     (format to-stream "~v@{~C~:*~}(" indent #\Space)
     (format to-stream "(("))
  (compile-type "" (car form) to-stream)
  (format to-stream ")")
  (compile-form (cadr form) nil 0 to-stream)
  (if stmtp
     (format to-stream ";~%")
     (format to-stream ")")))

(defun compile-switch (form indent to-stream)
  (format to-stream "~v@{~C~:*~}switch (" indent #\Space)
  (compile-form (car form) nil 0 to-stream)
  (format to-stream ")~%")
  (format to-stream "~v@{~C~:*~}  {~%" indent #\Space)
  (loop for (label clause) in (cdr form) do
        (if (consp label)
            (loop for l in label do
                  (if (string= (format nil "~a" l) "default")
                      (format to-stream "~v@{~C~:*~}default:~%" (+ 2 indent) #\Space)
                      (progn
                        (format to-stream "~v@{~C~:*~}case " (+ 2 indent) #\Space)
                        (compile-form l nil 0 to-stream)
                        (format to-stream ":~%"))))
          (progn
            (if (string= (format nil "~a" label) "default")
                (format to-stream "~v@{~C~:*~}default:~%" (+ 2 indent) #\Space)
              (progn
                (format to-stream "~v@{~C~:*~}case " (+ 2 indent) #\Space)
                (compile-form label nil 0 to-stream)
                (format to-stream ":~%")))))
        (compile-form clause t (+ indent 4) to-stream))
  (format to-stream "~v@{~C~:*~}  }~%" indent #\Space))

(defun compile-label (form indent to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "~a:~%" (car form)))

(defun compile-goto (form indent to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "goto ~a;~%" (car form)))

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
          ((string= "break"    (string op)) (compile-break args indent to-stream))
          ((string= "continue" (string op)) (compile-continue args indent to-stream))
          ((string= "cast"     (string op)) (compile-cast args stmtp indent to-stream))
          ((string= "def"      (string op)) (compile-def args stmtp indent to-stream))
          ((string= "seq"      (string op)) (compile-seq args stmtp indent to-stream))
          ((string= "progn"    (string op)) (compile-progn args indent to-stream))
          ((string= "addr"     (string op)) (compile-addr args indent to-stream))
          ((string= "deref"    (string op)) (compile-deref args indent to-stream))
          ((string= "."        (string op)) (compile-dot args stmtp indent to-stream))
          ((string= "->"       (string op)) (compile-arrow args stmtp indent to-stream))
          ((string= "aref"     (string op)) (compile-aref args indent to-stream))
          ((string= "if"       (string op)) (compile-if args indent to-stream))
          ((string= "label"    (string op)) (compile-label args indent to-stream))
          ((string= "goto"     (string op)) (compile-goto args indent to-stream))
          ((string= "?:"       (string op)) (compile-ite args stmtp indent to-stream))
          ((string= "for"      (string op)) (compile-for args indent to-stream))
          ((string= "do-while" (string op)) (compile-do-while args indent to-stream))
          ((string= "while"    (string op)) (compile-while args indent to-stream))
          ((string= "switch"   (string op)) (compile-switch args indent to-stream))
          ((string= "return"   (string op)) (compile-return args indent to-stream))
          ((member (string op) '("=" "+=" "-=" "*=" "/=" "%=" "&=" "^=" "|=" "<<=" ">>=")
                   :test #'equal)
           (compile-set args stmtp op indent to-stream))
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
                   (mapcar (lambda (x) (if (stringp x) (format nil "\"~a\"" x)
                                           (with-output-to-string (s) (compile-form x nil 0 s))))
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
