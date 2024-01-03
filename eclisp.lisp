(in-package #:eclisp)

(defvar macro-tbl (make-hash-table)
  "A global variable with the currently defined macros.")

(defvar macrofn-tbl (make-hash-table)
  "A global variable with the currently defined macrofns, this are functions
which operates directly on the AST.")

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
      (if (cdr args)
          (do ((cur args (cdr cur)))
              ((not cur))
              (funcall compile-proxy (car cur) nil 0 to-stream)
              (when (cdr cur)
                (format to-stream " ~a " op)))
        (progn
          (format to-stream "~a " op)
          (funcall compile-proxy (car args) nil 0 to-stream)))
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

(defvar c-keywords
  '("auto" "break" "case" "char" "const" "continue" "default" "do" "double"
    "else" "enum" "extern" "float" "for" "goto" "if" "int" "long" "register"
    "return" "short" "signed" "sizeof" "static" "struct" "switch" "typedef"
    "union" "unsigned" "void" "volatile" "while") )

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
                   (if (null (caddr type))
                       (if (consp (cadr type)) (cadr type) (cdr type))
                       (if (consp (caddr type)) (caddr type) (cddr type)))
                   (if (and (null acc) (string= name ""))
                       (list "["
                             (if (null (caddr type))
                                 ""
                                 (with-output-to-string
                                   (s)
                                   (compile-cpp-cond-expr (cadr type) nil 0 s)))
                             "]")
                     (list "(" acc name ")["
                           (if (null (caddr type))
                               ""
                               (with-output-to-string
                                 (s)
                                 (compile-cpp-cond-expr (cadr type) nil 0 s)))
                           "]"))))
    ((string= "->" (car type))
     (print-c-type ""
                   (cadr type)
                   (list "(" acc name ")("
                         ((lambda (l) (cons (cdar l) (cdr l)))
                          (loop for tt in (cddr type)
                                collect (list ", "
                                              (cond
                                               ((symbolp tt) (print-c-type "" tt nil))
                                               ((and (symbolp (car tt)) (not (member (string (car tt)) c-keywords
                                                                                     :test #'string=)))
                                                (let ((rev-tt (reverse tt)) (names nil) (typ nil))
                                                  (setf typ (car rev-tt))
                                                  (setf names (reverse (cdr rev-tt)))
                                                  ((lambda (l) (cons (cdar l) (cdr l)))
                                                   (loop for n in names
                                                         collect (list ", " (print-c-type n typ nil))))))
                                               ((and (symbolp (car tt)) (member (string (car tt)) c-keywords
                                                                                :test #'string=))
                                                (print-c-type "" tt nil))
                                               (t (print-c-type "" (car tt) nil))))))
                         ")")))
    ((string= "enum" (car type))
     (print-c-type name "enum "
                   (list (if (consp (cadr type))
                             (list #\Newline "{"
                                   ((lambda (l) (cons (cdar l) (cdr l)))
                                    (loop for tt in (cadr type)
                                          collect (list "," #\Newline
                                                        (if (consp tt)
                                                            (list "  " (car tt) " = " (cadr tt))
                                                          (list "  " tt)))))
                                   #\Newline "}")
                           (list (cadr type)
                                 (when (cddr type)
                                   (list #\Newline "{"
                                         ((lambda (l) (cons (cdar l) (cdr l)))
                                          (loop for tt in (cddr type)
                                                collect (list "," #\Newline
                                                              (if (consp tt)
                                                                  (list "  " (car tt) " = " (cadr tt))
                                                                (list "  " tt)))))
                                         #\Newline "}")))) acc)))
    ((string= "struct" (car type))
     (print-c-type name "struct "
                   (list (if (consp (cadr type))
                             (list "{" #\Newline
                                   (loop for tt in (cdr type)
                                         collect (list "  "
                                                       (if (symbolp (car tt))
                                                           (list
                                                            (when (and (cddr tt) (stringp (caddr tt)))
                                                              (with-output-to-string (s) (format s "/* ~a */~%" (caddr tt))))
                                                            (print-c-type (car tt) (cadr tt) nil))
                                                         (print-c-type "" (car tt) nil)) ";" #\Newline))
                                   "}")
                           (list (cadr type)
                                 (when (consp (caddr type))
                                   (list " {" #\Newline
                                         (loop for tt in (cddr type)
                                               collect (list "  "
                                                             (if (symbolp (car tt))
                                                                 (list
                                                                  (when (and (cddr tt) (stringp (caddr tt)))
                                                                    (with-output-to-string (s) (format s "/* ~a */~%" (caddr tt))))
                                                                  (print-c-type (car tt) (cadr tt) nil))
                                                               (print-c-type "" (car tt) nil)) ";" #\Newline))
                                         "}")))) acc)))
    ((string= "union" (car type))
     (print-c-type name "union "
                   (list (if (consp (cadr type))
                             (list "{" #\Newline
                                   (loop for tt in (cdr type)
                                         collect (list "  "
                                                       (if (symbolp (car tt))
                                                           (list
                                                            (when (and (cddr tt) (stringp (caddr tt)))
                                                              (with-output-to-string (s) (format s "/* ~a */~%" (caddr tt))))
                                                            (print-c-type (car tt) (cadr tt) nil))
                                                         (print-c-type "" (car tt) nil)) ";" #\Newline))
                                   "}")
                           (list (cadr type)
                                 (when (consp (caddr type))
                                   (list " {" #\Newline
                                         (loop for tt in (cddr type)
                                               collect (list "  "
                                                             (if (symbolp (car tt))
                                                                 (list
                                                                  (when (and (cddr tt) (stringp (caddr tt)))
                                                                    (with-output-to-string (s) (format s "/* ~a */~%" (caddr tt))))
                                                                  (print-c-type (car tt) (cadr tt) nil))
                                                               (print-c-type "" (car tt) nil)) ";" #\Newline))
                                         "}")))) acc)))
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
                                   acc)))
    ((string= "register" (car type))
     (list "register " (print-c-type name
                                   (if (consp (cadr type)) (cadr type) (cdr type))
                                   acc)))
    ((string= "extern" (car type))
     (list "extern " (print-c-type name
                                   (if (consp (cadr type)) (cadr type) (cdr type))
                                   acc)))
    ((string= "long" (car type))
     (list "long " (print-c-type name
                                 (if (consp (cadr type)) (cadr type) (cdr type))
                                 acc)))
    ((string= "short" (car type))
     (list "short " (print-c-type name
                                  (if (consp (cadr type)) (cadr type) (cdr type))
                                  acc)))
    ((string= "signed" (car type))
     (list "signed " (print-c-type name
                                  (if (consp (cadr type)) (cadr type) (cdr type))
                                  acc)))
    ((string= "unsigned" (car type))
     (list "unsigned " (print-c-type name
                                  (if (consp (cadr type)) (cadr type) (cdr type))
                                  acc)))
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
    (cond ((and type (string= (car type) "->"))
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
  (cond
   ((and stmtp (not (eql stmtp 'init)))
    (format to-stream "~v@{~C~:*~}" indent #\Space))
   ((not stmtp) (format to-stream "(")))
  (let ((cur form))
    (loop while cur do
          (compile-form (car cur) nil 0 to-stream)
          (if (cdr cur) (format to-stream " ~a " op))
          (setf cur (cdr cur))))
  (cond
   ((and stmtp (not (eql stmtp 'init)))
    (format to-stream ";~%"))
   ((not stmtp) (format to-stream ")"))))

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

(defun compile-list (form stmtp indent to-stream)
  (if (and stmtp (not (eql stmtp 'init)))
      (format to-stream "~v@{~C~:*~}" indent #\Space))
  (format to-stream "{")
  (let ((cur form))
    (loop while cur do
          (compile-form (car cur) 'init 0 to-stream)
          (if (cdr cur) (format to-stream ", "))
          (setf cur (cdr cur))))
  (format to-stream " }")
  (if (and stmtp (not (eql stmtp 'init)))
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
  (if (cadr form)
      (compile-form `(|progn| ,@(cdr form)) t (+ 2 indent) to-stream)
    (format to-stream ";~%")))

(defun compile-do-while (form indent to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "do")
  (format to-stream "~%")
  (compile-form `(|progn| ,(cadr form)) t (+ 2 indent) to-stream)
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
  (if (cadddr form)
      (compile-form `(|progn| ,@(cdddr form)) t (+ 2 indent) to-stream)
    (format to-stream ";~%")))

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
  (loop for label-clauses in (cdr form) do
        (destructuring-bind
         (label &rest clauses) label-clauses
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
         (compile-form `(|progn| ,@clauses) t (+ indent 4) to-stream)))
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

(defun compile-quote-macro (args ctx)
  args)

(defun compile-quote-as-c (args stmtp indent to-stream)
  (when stmtp
    (format to-stream "~v@{~C~:*~}" indent #\Space))
  (if (consp args)
      (progn
        (do* ((cur args (cdr cur)) (el (car cur) (car cur)))
             ((not cur))
          (if (consp el)
              (progn
                (if (and (or (stringp (car el)) (symbolp (car el))))
                    (cond ((string= (car el) "key")
                           (if (eql (cadr el) 'num)
                               (format to-stream "[~a] = " (caddr el))
                               (format to-stream ".~a = " (caddr el))))
                          ;; quote within quote should not happen in this mode,
                          ;; but you can still try
                          ((string= (car el) "quote") (format to-stream "~a" el))
                          (t
                           (format to-stream "{")
                           (compile-quote-as-c el nil (+ 2 indent) to-stream)
                           (format to-stream "}")
                           (when (cdr cur)
                             (format to-stream ", "))))
                    (progn
                      (format to-stream "{")
                      (compile-quote-as-c el nil (+ 2 indent) to-stream)
                      (format to-stream "}")
                      (when (cdr cur)
                        (format to-stream ", ")))))
              (progn
                (format to-stream "~a" el)
                (when (cdr cur)
                  (format to-stream ", "))))))
      (format to-stream "~a" args))
  (when stmtp
    (format to-stream ";~%")))

(defun compile-quote (args stmtp indent to-stream ctx)
  "This function is pretty different when operating from within macros where
it behaves like in other lisps.  However, when from outside macros, it expands
into the C-ish equivalent. C has something that looks like assoctiation lists"
  (if ctx
      (compile-quote-macro args ctx)
      (compile-quote-as-c args stmtp indent to-stream)))

(defun compile-backquote-macro (args ctx)
  (if (null args)
      ()
      (if (listp (car args))
          (if (and (or (stringp (caar args)) (symbolp (caar args))))
              (cond
                ((string= "unquote" (string (caar args)))
                 (cons (if (listp (cadar args))
                           (compile-macro (cadar args) ctx)
                           (gethash (cadar args) ctx))
                       (compile-backquote-macro (cdr args) ctx)))
                ((string= "unquote-splice" (string (caar args)))
                 (append (if (listp (cadar args))
                             (compile-macro (cadar args) ctx)
                             (gethash (cadar args) ctx))
                         (compile-backquote-macro (cdr args) ctx)))
                ((string= "quote" (string (caar args))) (cdar args))
                (t
                 (cons (compile-backquote-macro (car args) ctx)
                       (compile-backquote-macro (cdr args) ctx))))
              (cons (compile-backquote-macro (car args) ctx)
                    (compile-backquote-macro (cdr args) ctx)))
          (cons (car args) (compile-backquote-macro (cdr args) ctx)))))

(defun compile-backquote-as-c (args stmtp indent to-stream)
  (when stmtp
    (format to-stream "~v@{~C~:*~}" indent #\Space))
  (if (consp args)
      (progn
        (do* ((cur args (cdr cur)) (el (car cur) (car cur)))
             ((not cur))
          (if (consp el)
              (progn
                (if (and (or (stringp (car el)) (symbolp (car el))))
                    (cond ((string= (car el) "key")
                           (if (and (consp (caddr el))
                                    (or (stringp (caaddr el)) (symbolp (caaddr el)))
                                    (string= (caaddr el) "unquote"))
                               (if (eql (cadr el) 'num)
                                   (format to-stream "[~a] = " (with-output-to-string (s)
                                                                 (compile-form (car (cdaddr el)) nil 0 s)))
                                   (format to-stream ".~a = " (with-output-to-string (s)
                                                                (compile-form (car (cdaddr el)) nil 0 s))))
                               (if (eql (cadr el) 'num)
                                   (format to-stream "[~a] = " (caddr el))
                                   (format to-stream ".~a = " (caddr el)))))
                          ;; quote within quote should not happen in this mode,
                          ;; but you can still try
                          ((string= (car el) "quote") (format to-stream "~a" el))
                          ((string= (car el) "unquote")
                           (compile-form (cadr el) nil 0 to-stream)
                           (when (cdr cur)
                             (format to-stream ", ")))
                          (t
                           (format to-stream "{")
                           (compile-backquote-as-c el nil (+ 2 indent) to-stream)
                           (format to-stream "}")
                           (when (cdr cur)
                             (format to-stream ", "))))
                    (progn
                      (format to-stream "{")
                      (compile-backquote-as-c el nil (+ 2 indent) to-stream)
                      (format to-stream "}")
                      (when (cdr cur)
                        (format to-stream ", ")))))
              (progn
                (format to-stream "~a" el)
                (when (cdr cur)
                  (format to-stream ", "))))))
      (format to-stream "~a" args))
  (when stmtp
    (format to-stream ";~%")))

(defun compile-backquote (args stmtp indent to-stream ctx)
  "This function is pretty different when operating from within macros where
it behaves like in other lisps.  However, when from outside macros, it expands
into the C-ish equivalent. C has something that looks like assoctiation lists"
  (if ctx
      (compile-backquote-macro (car args) ctx)
      (compile-backquote-as-c args stmtp indent to-stream)))

(defun ctx-lookup (x ctx)
  (cond ((symbolp x) (gethash x ctx))
        ((listp x) (compile-macro x ctx))
        (t x)))

(defun compile-concat (typ args ctx)
  (let ((res
          (apply #'concatenate 'string
                 (mapcar #'(lambda (x) (format nil "~a" (ctx-lookup x ctx))) args))))
    (if (string= typ "string") res (intern res))))

(defun compile-car (args ctx)
  (car (ctx-lookup (car args) ctx)))

(defun compile-cdr (args ctx)
  (cdr (ctx-lookup (car args) ctx)))

(defun compile-null (args ctx)
  (null (gethash (car args) ctx)))

(defun compile-if-macro (args ctx)
  (let ((condition (ctx-lookup (car args) ctx))
        (then-branch (cadr args))
        (else-branch (caddr args)))
    (if condition
        (ctx-lookup then-branch ctx)
        (ctx-lookup else-branch ctx))))

(defun compile-listp (args ctx)
  (listp (ctx-lookup (car args) ctx)))

(defun compile-consp (args ctx)
  (consp (ctx-lookup (car args) ctx)))

(defun compile-symbolp (args ctx)
  (symbolp (ctx-lookup (car args) ctx)))

(defun compile-numberp (args ctx)
  (numberp (ctx-lookup (car args) ctx)))

(defun compile-stringp (args ctx)
  (stringp (ctx-lookup (car args) ctx)))

(defun compile-length (args ctx)
  (length (ctx-lookup (car args) ctx)))

(defun compile-op-macro (op args ctx)
  (cond
    ((string= op "+")
     (let (res)
       (setf res (apply #'+ (mapcar #'(lambda (x) (ctx-lookup x ctx)) args)))
       (if (not (integerp res)) (coerce res 'float) res)))
    ((string= op "-")
     (let (res)
       (setf res (apply #'- (mapcar #'(lambda (x) (ctx-lookup x ctx)) args)))
       (if (not (integerp res)) (coerce res 'float) res)))
    ((string= op "*")
     (let (res)
       (setf res (apply #'* (mapcar #'(lambda (x) (ctx-lookup x ctx)) args)))
       (if (not (integerp res)) (coerce res 'float) res)))
    ((string= op "/")
     (let (res)
       (setf res (apply #'/ (mapcar #'(lambda (x) (ctx-lookup x ctx)) args)))
       (if (not (integerp res)) (coerce res 'float) res)))
    ((string= op "%")
     (let ((res) (cur)
           (arg0 (ctx-lookup (car args) ctx))
           (arg1 (ctx-lookup (cadr args) ctx))
           (arg2+ (cddr args)))
       (if (null arg1)
           (setf res arg0)
           (progn
             (setf res (mod arg0 arg1))
             (do (cur arg2+ (cdr cur))
                 ((not cur))
               (setf res (mod res (ctx-lookup (car cur) ctx))))))
       (if (not (integerp res)) (coerce res 'float) res)))
    ;; todo: missing: ^ | & ~ << >>
    ((string= op "<")
     (apply #'< (mapcar #'(lambda (x) (ctx-lookup x ctx)) args)))
    ((string= op "<=")
     (apply #'<= (mapcar #'(lambda (x) (ctx-lookup x ctx)) args)))
    ((string= op ">")
     (apply #'> (mapcar #'(lambda (x) (ctx-lookup x ctx)) args)))
    ((string= op ">=")
     (apply #'> (mapcar #'(lambda (x) (ctx-lookup x ctx)) args)))
    ((string= op "&&")
     (let ((res t) (cur)
           (arg0 (ctx-lookup (car args) ctx))
           (arg1 (ctx-lookup (cadr args) ctx))
           (arg2+ (cddr args)))
           (setf res (and arg0 arg1))
       (do (cur arg2+ (cdr cur))
           ((or (not cur) (not res)))
         (setf res (and res (ctx-lookup (car cur) ctx))))
       res))
    ((string= op "||")
     (let ((res nil) (cur)
           (arg0 (ctx-lookup (car args) ctx))
           (arg1 (ctx-lookup (cadr args) ctx))
           (arg2+ (cddr args)))
           (setf res (or arg0 arg1))
       (do (cur arg2+ (cdr cur))
           ((or (not cur) res))
         (setf res (or res (ctx-lookup (car cur) ctx))))
       res))
    ((string= op "!=")
     (let ((arg0 (ctx-lookup (car args) ctx))
           (arg1 (ctx-lookup (cadr args) ctx)))
       ;; eql should be the least surprising
       (not (string= (format nil "~a" arg0) (format nil "~a" arg1)))))
    ((string= op "==")
     (let ((arg0 (ctx-lookup (car args) ctx))
           (arg1 (ctx-lookup (cadr args) ctx)))
       ;; eql should be the least surprising
       (string= (format nil "~a" arg0) (format nil "~a" arg1))))))

(defun compile-let-macro (args ctx)
  (let ((res nil) (tmp-bindings nil))
    (destructuring-bind (bindings &rest body) args
      (do ((bcur bindings (cdr bcur)))
          ((not bcur))
        (setf tmp-bindings (cons (list (caar bcur) (gethash (caar bcur) ctx)) tmp-bindings))
        (setf (gethash (caar bcur) ctx) (ctx-lookup (cadar bcur) ctx)))
      (setf res (compile-macro body ctx))
      ;; remove the bindings
      (do ((bcur tmp-bindings (cdr bcur)))
          ((not bcur))
        (remhash (caar bcur) ctx)
        (when (cadar bcur)
          (setf (gethash (caar bcur) ctx) (cadar bcur)))))
    res))

(defvar *eclisp-gensym-counter* 0)

(defun compile-gensym (args ctx)
  ;; todo: make something more robust
  (let ((prefix (car args)))
    (if (null prefix) (setf prefix "_G"))
    (make-symbol (concatenate 'string prefix
                              (format nil "_~a_" (get-universal-time))
                              (format nil "~a" (incf *eclisp-gensym-counter*))))))

(defun compile-macro (body ctx)
  (let ((res nil))
    (when (or (not (listp body)) (not (listp (car body))))
      (setf body (list body)))
    (loop for form in body do
      (setf res
            (if (consp body)
                (destructuring-bind (op &rest args) form
                  (cond
                    ((string= "backquote" (string op)) (compile-backquote args nil nil nil ctx))
                    ((string= "quote"     (string op)) (compile-quote args nil nil nil ctx))
                    ((string= "symbolp"   (string op)) (compile-symbolp args ctx))
                    ((string= "listp"     (string op)) (compile-listp args))
                    ((string= "car"       (string op)) (compile-car args ctx))
                    ((string= "cdr"       (string op)) (compile-cdr args ctx))
                    ((string= "null"      (string op)) (compile-null args ctx))
                    ((string= "consp"     (string op)) (compile-consp args ctx))
                    ((string= "stringp"   (string op)) (compile-stringp args ctx))
                    ((string= "length"    (string op)) (compile-length args ctx))
                    ((string= "numberp"   (string op)) (compile-numberp args ctx))
                    ((string= "concat"    (string op)) (compile-concat (car args) (cdr args) ctx))
                    ((string= "if"        (string op)) (compile-if-macro args ctx))
                    ((string= "gensym"    (string op)) (compile-gensym args ctx))
                    ((string= "let"       (string op)) (compile-let-macro args ctx))
                    ((member (string op) '("<" ">" "<=" ">=" ">" "==" "!=" "&&" "||") :test #'equal)
                     (compile-op-macro op args ctx))
                    ;; not implemented ^ | & ~ << >>
                    ((member (string op) '("+" "-" "*" "/" "%" "^" "|" "&" "~" "<<" ">>")
                             :test #'equal)
                     (compile-op-macro op args ctx))
                    (t (if (gethash op macrofn-tbl)
                           (eval-macrofn op args ctx)
                           (error (format nil "call to a C function (here, ~a) through the ffi is not yet unsupported.~%" op))))))
                (format t "unsupported: ~a~%" form))))
    res))

(defun register-macro (args)
  (let ((name nil) (macro-args nil) (documentation nil) (body nil))
    (setf name (car args))
    (setf macro-args (cadr args))
    (if (stringp (caddr args))
        (progn
          (setf documentation (caddr args))
          (setf body (cadddr args)))
        (setf body (caddr args)))
    (setf (gethash name macro-tbl) (list macro-args body))))

(defun register-macrofn (args)
  (let ((name nil) (macro-args nil) (documentation nil) (body nil))
    (setf name (car args))
    (setf macrofn-args (cadr args))
    (if (stringp (caddr args))
        (progn
          (setf documentation (caddr args))
          (setf body (cadddr args)))
        (setf body (caddr args)))
    (setf (gethash name macrofn-tbl) (list macrofn-args body))))

(defun expand-macro (macro args)
  (let ((ctx (make-hash-table))
        (tmpl (car (gethash macro macro-tbl)))
        (macro-body (cdr (gethash macro macro-tbl))))
    (do ((acur args (cdr acur))
         (tcur tmpl (cdr tcur)))
        ((not tcur))
      (if (string= (string (car tcur)) "&body")
          (progn
            (setf (gethash (cadr tcur) ctx) acur)
            (setf tcur (cdr tcur))
            (setf acur nil))
          (setf (gethash (car tcur) ctx) (car acur))))
      (compile-macro macro-body ctx)))

(defun eval-macrofn (fn args ctx-ref)
  (let ((ctx (make-hash-table))
        (tmpl (car (gethash fn macrofn-tbl)))
        (fn-body (cdr (gethash fn macrofn-tbl))))
    (do ((acur args (cdr acur))
           (tcur tmpl (cdr tcur)))
        ((not tcur))
      (if (string= (string (car tcur)) "&body")
          (progn
            (setf (gethash (cadr tcur) ctx) (ctx-lookup (car acur) ctx-ref))
            (setf tcur (cdr tcur))
            (setf acur nil))
          (setf (gethash (car tcur) ctx) (ctx-lookup (car acur) ctx-ref))))
    (compile-macro fn-body ctx)))

(defun compile-macrolet (args stmtp indent to-stream)
  (let ((res nil) (tmp-bindings nil))
    (destructuring-bind (bindings &rest body) args
      ;; augment the ctx with the let bindings
      (do ((bcur bindings (cdr bcur)))
          ((not bcur))
        (let ((name nil) (tmpl nil) (documentation nil) (ml-body nil))
          (setf name (caar bcur))
          (setf tmpl (cadar bcur))
          (if (stringp (caddar bcur))
              (progn
                (setf documentation (caddar bcur))
                (setf ml-body (car (cdddar bcur))))
              (setf ml-body (caddar bcur)))
          (setf tmp-bindings (cons (list name (gethash name macro-tbl)) tmp-bindings))
          (setf (gethash name macro-tbl) (list tmpl ml-body))))
      (setf res (compile-form body stmtp indent to-stream))
      ;; remove the bindings
      (do ((bcur tmp-bindings (cdr bcur)))
          ((not bcur))
        (remhash (caar bcur) macro-tbl)
        (when (cadar bcur)
          (setf (gethash (caar bcur) macro-tbl) (cadar bcur)))))
    res))

(defun compile-form (form stmtp indent to-stream)
  "Compile an eclisp FROM and write it on TO-STREAM"
  (if (consp form)
      (destructuring-bind (op &rest args) form
        (when (gethash op macro-tbl)
          (setf form (expand-macro op args))))
      (when (gethash form macro-tbl)
        (setf form (expand-macro form nil))))
  (when (or (not (listp form)) (not (listp (car form))))
    (setf form (list form)))
  (loop for f in form do
    (if (consp f)
        (destructuring-bind (op &rest args) f
          (cond
            ((string= "%include"  (string op)) (compile-cpp-include args indent to-stream))
            ((string= "%define"   (string op)) (compile-cpp-define args indent to-stream))
            ((string= "%if"       (string op)) (compile-cpp-if args indent to-stream))
            ((string= "%comment"  (string op)) (compile-comment args indent to-stream))
            ((string= "break"     (string op)) (compile-break args indent to-stream))
            ((string= "continue"  (string op)) (compile-continue args indent to-stream))
            ((string= "cast"      (string op)) (compile-cast args stmtp indent to-stream))
            ((string= "def"       (string op)) (compile-def args stmtp indent to-stream))
            ((string= "seq"       (string op)) (compile-seq args stmtp indent to-stream))
            ((string= "list"      (string op)) (compile-list args stmtp indent to-stream))
            ((string= "progn"     (string op)) (compile-progn args indent to-stream))
            ((string= "addr"      (string op)) (compile-addr args indent to-stream))
            ((string= "deref"     (string op)) (compile-deref args indent to-stream))
            ((string= "."         (string op)) (compile-dot args stmtp indent to-stream))
            ((string= "->"        (string op)) (compile-arrow args stmtp indent to-stream))
            ((string= "aref"      (string op)) (compile-aref args indent to-stream))
            ((string= "if"        (string op)) (compile-if args indent to-stream))
            ((string= "label"     (string op)) (compile-label args indent to-stream))
            ((string= "goto"      (string op)) (compile-goto args indent to-stream))
            ((string= "?:"        (string op)) (compile-ite args stmtp indent to-stream))
            ((string= "for"       (string op)) (compile-for args indent to-stream))
            ((string= "do-while"  (string op)) (compile-do-while args indent to-stream))
            ((string= "while"     (string op)) (compile-while args indent to-stream))
            ((string= "switch"    (string op)) (compile-switch args indent to-stream))
            ((string= "return"    (string op)) (compile-return args indent to-stream))
            ((string= "quote"     (string op)) (compile-quote args stmtp indent to-stream nil))
            ((string= "backquote" (string op)) (compile-backquote args stmtp indent to-stream nil))
            ((string= "macro"     (string op)) (register-macro args))
            ((string= "macrofn"   (string op)) (register-macrofn args))
            ((string= "macrolet"  (string op)) (compile-macrolet args stmtp indent to-stream))
            ((member (string op) '("=" "+=" "-=" "*=" "/=" "%=" "&=" "^=" "|=" "<<=" ">>=")
                     :test #'equal)
             (compile-set args stmtp op indent to-stream))
            ((member (string op) '("<" ">" "<=" ">=" ">" "==" "!=" "&&" "||") :test #'equal)
             (compile-cmp-op f indent to-stream))
            ((member (string op) '("+" "-" "*" "/" "%" "^" "|" "&" "~" "<<" ">>")
                     :test #'equal)
             (compile-arith-binop f indent to-stream)
             (when (and stmtp (not (eql stmtp 'init))) (format to-stream ";~%")))
            (t
             (format to-stream "~v@{~C~:*~}" indent #\Space)
             (format to-stream "~a (~{~a~^, ~})"
                     (car f)
                     (mapcar (lambda (x) (if (stringp x) (format nil "\"~a\"" x)
                                             (with-output-to-string (s) (compile-form x nil 0 s))))
                             (cdr f)))
             (when stmtp (format to-stream ";~%")))))
        (progn
          (format to-stream "~v@{~C~:*~}" indent #\Space)
          (format to-stream "~a" f)))))

(defun compile-eclisp (from-stream to-stream)
  "Write the result of the compilation of the content of FROM-STREAM into
TO-STREAM."
  (loop for form = (parse from-stream)
        while form do (compile-form form t 0 to-stream)))

(defun main ()
  "The entry point."
  (compile-eclisp *standard-input* *standard-output*))
