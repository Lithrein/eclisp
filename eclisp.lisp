;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

(in-package #:eclisp)

(defvar macro-tbl (make-hash-table)
  "A global variable with the currently defined macros.")

(defvar macrofn-tbl (make-hash-table)
  "A global variable with the currently defined macrofns, this are functions
which operates directly on the AST.")

(defun print-cpp-include (include-forms stmtp indent to-stream)
  "Compile an include directive: (%include header-list)
where header-list is a list of header names either enclosed in double-quotes
or in angle-brackets."
  (declare (ignore stmtp))
  (pop include-forms)
  (loop for filename in include-forms do
        (format to-stream "~v@{~C~:*~}" indent #\Space)
        (format to-stream
                (cond ((symbolp filename) "#include ~a~%")
                      (t "#include \"~a\"~%"))
                filename)))

(defun print-cpp-define (form stmtp indent to-stream)
  "Compile a define directive: %(define name substitution) and write it
to TO-STREAM. FORM is a cons made of NAME and SUBSTITUTION.
NAME and SUBSTITUTION are both strings and should enclosed in double-quotes.
NAME can contain parenthesis just like a C macro does, i.e., no spaces before
the parenthesis.  SUBSTITUTION should be valid C code."
  (declare (ignore stmtp))
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "#define ~{~a~^ ~}~%" form))

(defun print-verbatim (form stmtp indent to-stream)
  (declare (ignore indent))
  (pop form)
  (format to-stream "~a" (car form))
  (when stmtp (format to-stream "~%")))

(defun compile-verbatim (form) form)

(defun print-arith-binop (form stmtp indent to-stream &optional (cpp nil))
  "Compile a FORM beginning with an arithmetic operator (+ - * / % ^ | || & &&
~ << >>) into a valid C expression and write it on TO-STREAM.
The CPP parameter enable C Preprocessor mode, restricting what can appear in
the expression."
  (let ((print-proxy (if cpp #'print-cpp-cond-expr #'print-form)))
    (destructuring-bind (op &rest args) form
      (format to-stream "~v@{~C~:*~}" indent #\Space)
      (format to-stream "(")
      (if (cdr args)
          (do ((cur args (cdr cur)))
              ((not cur))
            (funcall print-proxy (car cur) nil 0 to-stream)
            (when (cdr cur)
              (format to-stream " ~a " op)))
          (progn
            (format to-stream "~a " op)
            (funcall print-proxy (car args) nil 0 to-stream)))
      (format to-stream ")")))
  (when stmtp (format to-stream ";~%")))

(defun print-cmp-op (form stmtp indent to-stream &optional (cpp nil))
  "Compile a FORM beginning with a compare operator (< > <= >= > ==) into a valid
C expression and write it on TO-STREAM.
The CPP parameter enable C Preprocessor mode, restricting what can appear in
the expression."
  (declare (ignore stmtp))
  (let ((print-proxy (if cpp #'print-cpp-cond-expr #'print-form)))
    (destructuring-bind (op &rest args) form
      (format to-stream "(")
      (do ((cur args (cdr cur)))
          ((not (cdr cur)))
        (format to-stream "(")
        (funcall print-proxy (car cur) nil indent to-stream)
        (format to-stream " ~a " op)
        (funcall print-proxy (cadr cur) nil indent to-stream)
        (format to-stream ")")
        (when (cddr cur)
          (format to-stream " && ")))
      (format to-stream ")"))))

(defun print-cpp-cond-expr (form stmtp indent to-stream)
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
           (print-cmp-op form stmtp indent to-stream t))
          ((member (string op) '("!" "+" "-" "*" "/" "%" "^" "|" "||" "&" "&&" "~" "<<" ">>")
                   :test #'equal)
           (print-arith-binop form stmtp 0 to-stream t))))
      (format to-stream "~a" form)))

(defun print-cond-expr (form stmtp indent to-stream)
  "Compile a condition FORM, i.e.,  one that may appear after
and if, and write it on TO-STREAM."
  (declare (ignore indent))
  (if (consp form)
      (destructuring-bind (op &rest args) form
        (cond
          ((member (string op) '("<" ">" "<=" ">=" ">" "==" "!=") :test #'equal)
           (print-cmp-op form stmtp indent to-stream))
          ((member (string op) '("+" "-" "*" "/" "%" "^" "|" "||" "&" "&&" "~" "<<" ">>")
                   :test #'equal)
           (print-arith-binop form stmtp 0 to-stream))))
      (format to-stream "~a" form)))

(defun print-cpp-if (form stmtp indent to-stream)
  "Compile a if preprocessor directive FORM and write it on TO-STREAM"
  (pop form)
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
      (unless (and (symbolp cond) (string= cond "t")) (print-cpp-cond-expr cond nil 0 to-stream))
      (format to-stream "~%")
      (loop for b in body do
            (print-form b t (+ 2 indent) to-stream))))
  (format to-stream "~%~v@{~C~:*~}#endif~%" indent #\Space))

(defun compile-cpp-if (form)
  "Compile a if preprocessor directive FORM and write it on TO-STREAM"
  (let ((res))
    (do ((cur form (cdr cur)) (first t nil))
        ((not cur))
      (destructuring-bind (cond &rest body) (car cur)
        (push (list* (compile-form cond)
                     (loop for b in body collect (compile-form b))) res)))
    (reverse res)))

(defun print-ll (l shift to-stream)
  "Flatten and print to TO-STREAM the list produced by PRINT-C-TYPE."
  (cond ((null l) nil)
        ((consp l)
         (cond ((listp (car l)) (print-ll (car l) shift to-stream))
               (t
                (when (stringp (car l))
                  (setf shift (cond ((string= (car l) "{") (+ shift 2))
                                    ((string= (car l) "}") (- shift 2))
                                    (t shift))))
                (cond ((string= (car l) #\Newline)
                       (format to-stream "~a" (car l))
                       (setf shift (if (and (consp (cdr l)) (stringp (cadr l))
                                            (string= (cadr l) "}"))
                                       (- shift 2)
                                       shift))
                       (format to-stream "~v@{~C~:*~}" shift #\Space))
                      (t (format to-stream "~a" (car l))))))
         (print-ll (cdr l) shift to-stream))))

(defvar +c-keywords+
  '("auto" "break" "case" "char" "const" "continue" "default" "do" "double"
    "else" "enum" "extern" "float" "for" "goto" "if" "int" "long" "register"
    "return" "short" "signed" "sizeof" "static" "struct" "switch" "typedef"
    "union" "unsigned" "void" "volatile" "while"))

(defun group-bitfield (lst)
  (unless (null lst)
    (if (symbolp (cadr lst))
        (cons (list (car lst)) (group-bitfield (cdr lst)))
        (cons (list (car lst) (cadr lst)) (group-bitfield (cddr lst))))))

(defvar +array-kwd+ "array"
  "The keyword used to declare arrays.")
(defvar +ptr-kwd+ "ptr"
  "The keyword used to declare pointers.")
(defvar +fun-kwd+ "->"
  "The keyword to declare functions.")

(defun print-c-type (name type acc)
  "Create a nested list which represents the variable NAME of TYPE.
ACC should be NIL at first."
  (if (consp type)
      (let ((kind (car type)))
        (cond
          ((string= +ptr-kwd+ kind)
           (print-c-type nil
                         (if (consp (cadr type)) (cadr type) (cdr type))
                         (list "(*" acc name ")")))
          ((and (string= +array-kwd+ kind) (cdr type))
           (print-c-type nil
                         (if (null (caddr type))
                             (if (consp (cadr type)) (cadr type) (cdr type))
                             (if (consp (caddr type)) (caddr type) (cddr type)))
                         (append
                          (if (and (null acc) (null name)) nil (list "(" acc name ")"))
                          (list "["
                                (if (null (caddr type))
                                    ""
                                    (with-output-to-string (s)
                                      ;; c89: (print-cpp-cond-expr (cadr type) nil 0 s)))
                                      (print-form (cadr type) nil 0 s)))
                                "]"))))
          ((string= +fun-kwd+ kind)
           (print-c-type nil
                         (cadr type)
                         (list "(" acc name ")("
                               ((lambda (l) (cons (cdar l) (cdr l)))
                                (loop for tt in (cddr type)
                                      collect (list ", "
                                                    (cond
                                                      ((symbolp tt) (print-c-type nil tt nil))
                                                      ((and (symbolp (car tt)) (not (member (string (car tt)) +c-keywords+
                                                                                            :test #'string=)))
                                                       (let ((rev-tt (reverse tt)) (names nil) (typ nil))
                                                         (setf typ (car rev-tt))
                                                         (setf names (reverse (cdr rev-tt)))
                                                         ((lambda (l) (cons (cdar l) (cdr l)))
                                                          (loop for n in names
                                                                collect (list ", " (print-c-type n typ nil))))))
                                                      ((and (symbolp (car tt)) (member (string (car tt)) +c-keywords+
                                                                                       :test #'string=))
                                                       (print-c-type nil tt nil))
                                                      (t (print-c-type nil (car tt) nil))))))
                               ")")))
          ((string= "enum" kind)
           (print-c-type name (string kind)
                         (let* ((enum-anon-p (consp (cadr type)))
                                (enum-name (if enum-anon-p nil (list (cadr type))))
                                (enum-contents (if enum-anon-p (cadr type) (cddr type))))
                           (append enum-name
                                   (when enum-contents
                                     (append
                                      (when enum-name (list #\Newline))
                                      (list "{")
                                      ((lambda (l) (cons (cdar l) (cdr l)))
                                       (loop for tt in enum-contents
                                             collect (list "," #\Newline
                                                           (if (consp tt)
                                                               (list (car tt) " = " (cadr tt))
                                                               (list tt)))))
                                      (list #\Newline "}")))
                                   acc))))
          ((member kind '("struct" "union") :test #'string=)
           (print-c-type name (string kind)
                         (let* ((struct-anon-p (consp (cadr type)))
                                (struct-name (if struct-anon-p nil (list (cadr type))))
                                (struct-contents (if struct-anon-p (cdr type) (cddr type))))
                           (append struct-name
                                   (when struct-contents
                                     (append
                                      (when struct-name (list #\Newline))
                                      (list "{")
                                      ((lambda (l) (append (cons (cdar l) (cdr l)) (list ";")))
                                       (loop for tt in struct-contents
                                             collect (list ";" #\Newline
                                                           (cond ((and (symbolp (car tt)) (not (member (string (car tt)) +c-keywords+ :test #'string=)))
                                                                  (let* ((rev-tt (reverse tt))
                                                                         (documentation (when (stringp (car rev-tt)) (car rev-tt)))
                                                                         (typ (if documentation (cadr rev-tt) (car rev-tt)))
                                                                         (ntt (if documentation (reverse (cddr rev-tt)) (reverse (cdr rev-tt)))))
                                                                    (append
                                                                     (when documentation (list (with-output-to-string (s)
                                                                                                 (format s "/* ~a */" documentation))
                                                                                               #\Newline))
                                                                     ((lambda (l) (cons (cddar l) (cdr l)))
                                                                      (loop for var in (group-bitfield ntt)
                                                                            collect (list ";" #\Newline
                                                                                          (print-c-type (if (cadr var)
                                                                                                            (format nil "~a : ~a" (car var)
                                                                                                                    (with-output-to-string (s)
                                                                                                                      (print-form (cadr var) nil 0 s)))
                                                                                                            (car var))
                                                                                                        typ nil)))))))
                                                                 ((and (symbolp (car tt)) (member (string (car tt)) +c-keywords+ :test #'string=))
                                                                  (let* ((rev-tt (reverse tt))
                                                                         (documentation (when (stringp (car rev-tt)) (car rev-tt)))
                                                                         (ntt (if documentation (reverse (cdr rev-tt)))))
                                                                    (append
                                                                     (when documentation (list (with-output-to-string (s)
                                                                                                 (format s "/* ~a */" documentation))
                                                                                               #\Newline))
                                                                     (print-c-type nil ntt nil))))
                                                                 (t
                                                                  (print-c-type nil (car tt) nil))))))
                                      (list #\Newline "}")))
                                   acc))))
          ((member kind '("volatile" "const") :test #'string=)
           (print-c-type nil (if (consp (cadr type)) (cadr type) (cdr type)) (list (string kind) " " acc name)))
          ((member kind '("typedef" "static" "register" "extern" "long" "short" "signed" "unsigned") :test #'string=)
           (append (list kind " ") (print-c-type name (if (consp (cadr type)) (cadr type) (cdr type)) acc)))
          (t (append (list kind) (if (null acc) nil (list " ")) acc (if (null name) nil (list " ")) (list name)))))
      (append (list type) (if (null acc) nil (list " ")) acc (if (null name) nil (list " ")) (list name))))

(defun print-type (name type to-stream)
  "Compile TYPE and write it on TO-STREAM."
  (print-ll (print-c-type name type nil) 0 to-stream))

(defun print-as-type (form stmtp indent to-stream)
  (when stmtp
    (format to-stream "~v@{~C~:*~}{~%" indent #\Space))
  (format to-stream "~a" (with-output-to-string (s) (print-type "" (cadr form) s)))
  (when stmtp
    (format to-stream ";~%")))

(defun print-progn (form stmtp indent to-stream)
  (pop form)
  (cond ((>= indent 0)
         (format to-stream "~v@{~C~:*~}{~%" indent #\Space)
         (loop for f in form do (print-form f t (+ 2 indent) to-stream))
         (format to-stream "~v@{~C~:*~}}~%" indent #\Space))
        (t (loop for f in form do (print-form f t indent to-stream)))))

(defun print-funcall (form stmtp indent to-stream)
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "~a (~{~a~^, ~})"
          (car form)
          (mapcar (lambda (x) (if (stringp x) (format nil "\"~a\"" x)
                                  (with-output-to-string (s) (print-form x nil 0 s))))
                  (cdr form)))
  (when stmtp (format to-stream ";~%")))

(defun print-defvar (form stmtp indent to-stream)
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
    (print-type var (if (null type) '(|int|) type) to-stream)
    (when value (format to-stream " = ~a"
                        (with-output-to-string (s) (print-form value nil 0 s))))
    (when stmtp (format to-stream ";~%"))))

(defun compile-defvar (form)
  "Compile a form which declares a global variable.
                  (def var type value documentation)"
  (let ((var nil) (type nil) (value nil) (documentation nil))
    (cond ((and (consp (car form)) (member (caar form) +c-keywords+ :test #'string=))
           ;; no name, hence no value
           (setf type (car form))
           (setf documentation (cadr form)))
          ;; name is present
          (t
           (setf var (car form))
           (if (consp (cadr form))
               (progn
                 (setf type (cadr form))
                 (setf value (caddr form))
                 (setf documentation (cadddr form)))
               (progn
                 (setf value (cadr form))
                 (setf documentation (caddr form))))))
    (list var
          (if (null type) '(|int|) type)
          (when value (compile-form value))
          documentation)))

(defun print-defun (form stmtp indent to-stream)
  "Compile a form which declares a global variable.
                  (def f (-> rettype params) documentation body)
BODY is optional. DOCUMENTATION is optional"
  (let ((var nil) (type nil) (body nil) (documentation nil))
    (setf var (car form))
    (setf type (cadr form))
    (setf documentation (caddr form))
    (setf body (cdddr form))
    (unless (null documentation)
      (progn
        (format to-stream "~v@{~C~:*~}" indent #\Space)
        (format to-stream "/* ~a  */~%"
                (regex-replace-all "\\n" documentation
                                   (concatenate 'string '(#\Newline) "   "
                                                (format nil "~v@{~C~:*~}" indent #\Space))))))
    (format to-stream "~v@{~C~:*~}" indent #\Space)
    (print-type var (if (null type) '(void) type) to-stream)
    (when body
      (format to-stream "~%")
      (print-progn (list* '|progn| body) stmtp indent to-stream))
    (when (not body)
      (format to-stream ";~%"))))

(defun compile-defun (form)
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
    (list* var type documentation
           (when body
             (mapcar (lambda (form) (compile-form form)) body)))))

(defun print-def (form stmtp indent to-stream)
  (pop form)
  (let ((category nil) (var nil) (type nil))
    (if (consp (car form))
        (setf type (car form))
        ;; name is present
        (progn
          (setf var (car form))
          (when (consp (cadr form)) (setf type (cadr form)))))
    (cond ((and type (string= (car type) "->"))
           (print-defun form stmtp indent to-stream))
          (t (print-defvar form stmtp indent to-stream)))))

 (defun compile-def (form)
   (let ((var nil) (type nil))
     (cond ((and (consp (car form)) (member (caar form) +c-keywords+ :test #'string=))
            (setf type (car form)))
           (t (setf name (car form)
                    type (when (consp (cadr form)) (cadr form)))))
     (cond ((and type (string= (car type) "->"))
            (compile-defun form))
           (t (compile-defvar form)))))

(defun print-if (form stmtp indent to-stream)
  (pop form)
  (format to-stream "~v@{~C~:*~}if (" indent #\Space)
  (print-form (car form) nil 0 to-stream)
  (format to-stream ")~%")
  (print-form (cadr form) t (+ 2 indent) to-stream)
  (unless (null (caddr form))
    (format to-stream "~v@{~C~:*~}else ~%" indent #\Space)
    (print-form (caddr form) t (+ 2 indent) to-stream)))

(defun print-set (form stmtp indent to-stream)
  (destructuring-bind (op &rest args) form
    (cond
      (stmtp (format to-stream "~v@{~C~:*~}" indent #\Space))
      ((not stmtp) (format to-stream "(")))
    (let ((cur args))
      (loop while cur do
            (print-form (car cur) nil 0 to-stream)
            (if (cdr cur) (format to-stream " ~a " op))
            (setf cur (cdr cur))))
    (cond
      (stmtp (format to-stream ";~%"))
      ((not stmtp) (format to-stream ")")))))

(defun print-seq (form stmtp indent to-stream)
  (pop form)
  (if stmtp
      (format to-stream "~v@{~C~:*~}" indent #\Space))
  (let ((cur form))
    (loop while cur do
          (print-form (car cur) nil 0 to-stream)
          (if (cdr cur) (format to-stream ", "))
          (setf cur (cdr cur))))
  (if stmtp
      (format to-stream ";~%")))

(defun print-list (form stmtp indent to-stream)
  (pop form)
  (if (and stmtp)
      (format to-stream "~v@{~C~:*~}" indent #\Space))
  (format to-stream "{")
  (let ((cur form))
    (loop while cur do
          (when (and (consp (car cur))
                     (or (stringp (caar cur)) (symbolp (caar cur)))
                     (string= (caar cur) "%key"))
            (format to-stream (if (eql (cadar cur) 'num) "[~a] = " ".~a = ")
                    (with-output-to-string (s) (print-form (caddar cur) nil 0 s)))
            (setf cur (cdr cur)))
          (print-form (car cur) nil 0 to-stream)
          (if (cdr cur) (format to-stream ", "))
          (setf cur (cdr cur))))
  (format to-stream " }")
  (if (and stmtp)
      (format to-stream ";~%")))

(defun print-dot (form stmtp indent to-stream)
  (pop form)
  (if stmtp
      (format to-stream "~v@{~C~:*~}" indent #\Space))
  (let ((cur form))
    (loop while cur do
          (print-form (car cur) nil 0 to-stream)
          (if (cdr cur) (format to-stream "."))
          (setf cur (cdr cur))))
  (if stmtp
      (format to-stream ";~%")))

(defun print-arrow (form stmtp indent to-stream)
  (pop form)
  (if stmtp
      (format to-stream "~v@{~C~:*~}" indent #\Space))
  (let ((cur form))
    (loop while cur do
          (print-form (car cur) nil 0 to-stream)
          (if (cdr cur) (format to-stream "->"))
          (setf cur (cdr cur))))
  (if stmtp
      (format to-stream ";~%")))

(defun print-aref (form stmtp indent to-stream)
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (print-form (car form) nil indent to-stream)
  (let ((cur (cdr form)))
    (loop while cur do
          (format to-stream "[")
          (print-form (car cur) nil 0 to-stream)
          (format to-stream "]")
          (setf cur (cdr cur)))))

(defun print-addr (form stmtp indent to-stream)
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "&")
  (print-form (car form) nil indent to-stream))

(defun print-deref (form stmtp indent to-stream)
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "*")
  (print-form (car form) nil indent to-stream))

(defun print-while (form stmtp indent to-stream)
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "while (")
  (print-form (car form) nil 0 to-stream)
  (format to-stream ")~%")
  (if (cadr form)
      (print-form `(|progn| ,@(cdr form)) t (+ 2 indent) to-stream)
      (format to-stream ";~%")))

(defun print-do-while (form stmtp indent to-stream)
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "do")
  (format to-stream "~%")
  (print-form `(|progn| ,(cadr form)) t (+ 2 indent) to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "while ")
  (print-cond-expr (car form) stmtp 0 to-stream)
  (format to-stream ";~%"))

(defun print-ite (form stmtp indent to-stream)
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (print-cond-expr (car form) stmtp 0 to-stream)
  (format to-stream " ? ")
  (print-form (cadr form) nil 0 to-stream)
  (format to-stream " : ")
  (print-form (caddr form) nil 0 to-stream)
  (when stmtp (format to-stream ";~%")))

(defun print-for (form stmtp indent to-stream)
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "for (")
  (print-form (car form) nil 0 to-stream)
  (format to-stream "; ")
  (print-cond-expr (cadr form) nil 0 to-stream)
  (format to-stream "; ")
  (print-form (caddr form) nil 0 to-stream)
  (format to-stream ")~%")
  (if (cadddr form)
      (print-form `(|progn| ,@(cdddr form)) t (+ 2 indent) to-stream)
      (format to-stream ";~%")))

(defun print-return (form stmtp indent to-stream)
  (declare (ignore stmtp))
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "return ")
  (print-form (car form) nil 0 to-stream)
  (format to-stream ";~%"))

(defun print-break (form stmtp indent to-stream)
  (declare (ignore stmtp))
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "break;~%"))

(defun print-continue (form stmtp indent to-stream)
  (declare (ignore stmtp))
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "continue;~%"))

(defun print-cast (form stmtp indent to-stream)
  (pop form)
  (if stmtp
      (format to-stream "~v@{~C~:*~}" indent #\Space)
      (format to-stream "("))
  (do ((cur form (cdr cur)))
      ((not (cdr cur)))
    (format to-stream "(")
    (print-type nil (car cur) to-stream)
    (format to-stream ")"))
  (print-form (car (reverse form)) nil 0 to-stream)
  (if stmtp
      (format to-stream ";~%")
      (format to-stream ")")))

(defun print-switch (form stmtp indent to-stream)
  (pop form)
  (format to-stream "~v@{~C~:*~}switch (" indent #\Space)
  (print-form (car form) nil 0 to-stream)
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
                          (print-form l nil 0 to-stream)
                          (format to-stream ":~%"))))
              (progn
                (if (string= (format nil "~a" label) "default")
                    (format to-stream "~v@{~C~:*~}default:~%" (+ 2 indent) #\Space)
                    (progn
                      (format to-stream "~v@{~C~:*~}case " (+ 2 indent) #\Space)
                      (print-form label nil 0 to-stream)
                      (format to-stream ":~%")))))
          (print-form `(|progn| ,@clauses) t (+ indent 4) to-stream)))
  (format to-stream "~v@{~C~:*~}  }~%" indent #\Space))

(defun compile-switch (form)
  (list* (car form)
         (loop for label-clauses in (cdr form) collect
               (destructuring-bind
                     (label &rest clauses) label-clauses
                 (list* (if (consp label)
                           (loop for l in label collect
                                 (if (string= (format nil "~a" l) "default")
                                     '|default|
                                     (compile-form l)))
                           (progn
                             (if (string= (format nil "~a" label) "default")
                                 '|default|
                                 (compile-form label))))
                       (mapcar (lambda (form) (compile-form form)) clauses))))))

(defun print-label (form stmtp indent to-stream)
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "~a:~%" (car form)))

(defun print-goto (form stmtp indent to-stream)
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "goto ~a;~%" (car form)))

(defun print-comment (form stmtp indent to-stream)
  (declare (ignore stmtp))
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "//~a~%" (car form)))

(defun compile-quote (args ctx)
  (declare (ignore ctx))
  (car args))

(defun compile-quote-c (args)
  (car args))

(defun print-quote (args stmtp indent to-stream)
  (pop args)
  (print-quote-aux args stmtp indent to-stream))

(defun print-quote-aux (args stmtp indent to-stream)
  (when stmtp
    (format to-stream "~v@{~C~:*~}" indent #\Space))
  (if (consp args)
      (progn
        (do* ((cur args (cdr cur)) (el (car cur) (car cur)))
             ((not cur))
          (if (consp el)
              (progn
                (if (and (or (stringp (car el)) (symbolp (car el))))
                    (cond ((string= (car el) "%key")
                           (if (eql (cadr el) 'num)
                               (format to-stream "[~a] = " (caddr el))
                               (format to-stream ".~a = " (caddr el))))
                          ;; quote within quote should not happen in this mode,
                          ;; but you can still try
                          ((string= (car el) "quote") (format to-stream "~a" el))
                          (t
                           (format to-stream "{")
                           (print-quote-aux el nil (+ 2 indent) to-stream)
                           (format to-stream "}")
                           (when (cdr cur)
                             (format to-stream ", "))))
                    (progn
                      (format to-stream "{")
                      (print-quote-aux el nil (+ 2 indent) to-stream)
                      (format to-stream "}")
                      (when (cdr cur)
                        (format to-stream ", ")))))
              (progn
                (format to-stream (if (stringp el) "\"~a\"" "~a") el)
                (when (cdr cur)
                  (format to-stream ", "))))))
      (format to-stream (if (stringp args) "\"~a\"" "~a") args))
  (when stmtp
    (format to-stream ";~%")))

;; cf. The two following functions have been adapted from CLtL2 Appendix C.
(defun bracket (x)
  (cond ((atom x)
         (list '|list| (compile-backquote-aux x)))
        ((eq (car x) '|unquote|)
         (list '|list| (cadr x)))
        ((eq (car x) '|unquote-splice|)
         (cadr x))
        (t (list '|list| (compile-backquote-aux x)))))

(defun compile-backquote-aux (args)
  (cond ((atom args) (list '|quote| args))
        ((eq (car args) '|backquote|) (compile-backquote-aux (compile-backquote-aux (cadr args))))
        ((eq (car args) '|unquote|) (cadr args))
        ((eq (car args) '|unquote-splice|) (error ",@~S after `" (cadr args)))
        (t (do ((p args (cdr p))
                (q '() (cons (bracket (car p)) q)))
               ((atom p) (cons '|append| (nreconc q (list (list '|quote| p)))))
             (when (eq (car p) '|unquote|)
               (unless (null (cddr p)) (error "Malformed ,~S" p))
               (return (cons '|append| (nreconc q (list (cadr p))))))
             (when (eq (car p) '|unquote-splice|)
               (error "Dotted ,@~S" p))))))

(defun compile-backquote (args ctx)
  ;; todo: investigate why the need for car here
  (car (compile-macro (compile-backquote-aux args) ctx)))

(defun print-backquote (args stmtp indent to-stream)
  (pop args)
  (print-backquote-aux args stmtp indent to-stream))

(defun print-backquote-aux (args stmtp indent to-stream)
  (when stmtp
    (format to-stream "~v@{~C~:*~}" indent #\Space))
  (if (consp args)
      (progn
        (do* ((cur args (cdr cur)) (el (car cur) (car cur)))
             ((not cur))
          (if (consp el)
              (progn
                (if (and (or (stringp (car el)) (symbolp (car el))))
                    (cond ((string= (car el) "%key")
                           (if (and (consp (caddr el))
                                    (or (stringp (caaddr el)) (symbolp (caaddr el)))
                                    (string= (caaddr el) "unquote"))
                               (if (eql (cadr el) 'num)
                                   (format to-stream "[~a] = " (with-output-to-string (s)
                                                                 (print-form (car (cdaddr el)) nil 0 s)))
                                   (format to-stream ".~a = " (with-output-to-string (s)
                                                                (print-form (car (cdaddr el)) nil 0 s))))
                               (if (eql (cadr el) 'num)
                                   (format to-stream "[~a] = " (caddr el))
                                   (format to-stream ".~a = " (caddr el)))))
                          ;; quote within quote should not happen in this mode,
                          ;; but you can still try
                          ((string= (car el) "quote") (format to-stream "~a" el))
                          ((string= (car el) "unquote")
                           (print-form (cadr el) nil 0 to-stream)
                           (when (cdr cur)
                             (format to-stream ", ")))
                          (t
                           (format to-stream "{")
                           (print-backquote-aux el nil (+ 2 indent) to-stream)
                           (format to-stream "}")
                           (when (cdr cur)
                             (format to-stream ", "))))
                    (progn
                      (format to-stream "{")
                      (print-backquote-aux el nil (+ 2 indent) to-stream)
                      (format to-stream "}")
                      (when (cdr cur)
                        (format to-stream ", ")))))
              (progn
                (format to-stream (if (stringp el) "\"~a\"" "~a") el)
                (when (cdr cur)
                  (format to-stream ", "))))))
      (format to-stream (if (stringp args) "\"~a\"" "~a") args))
  (when stmtp
    (format to-stream ";~%")))


(defun compile-backquote-c (args)
  (compile-backquote-c-aux (car args)))

(defun compile-backquote-c-aux (args)
  (let ((res nil))
    (if (consp args)
        (progn
          (do* ((cur args (cdr cur)) (el (car cur) (car cur)))
               ((not cur))
            (if (consp el)
                (progn
                  (if (and (or (stringp (car el)) (symbolp (car el))))
                      (cond
                            ;; quote within quote should not happen in this mode,
                            ;; but you can still try
                            ((string= (car el) "quote") (push el res))
                            ((string= (car el) "unquote")
                             (push `(|unquote| ,(compile-form (cadr el))) res))
                            (t (push (compile-backquote-c-aux el) res)))
                      (push (compile-backquote-c-aux el) res)))
                (push el res))))
        (setf res args))
    (if (listp res) (reverse res) res)))


(defun ctx-lookup (x ctx)
  (cond ((symbolp x) (gethash x ctx))
        ((listp x) (compile-macro x ctx))
        (t x)))

(defun compile-concat (args ctx)
  (let* ((typ (car args))
         (cargs (cdr args))
         (res
          (apply #'concatenate 'string
                 (mapcar #'(lambda (x) (format nil "~a" (ctx-lookup x ctx))) cargs))))
    (if (string= typ "string") res (intern res))))

(defun compile-car (args ctx)
  (car (ctx-lookup (car args) ctx)))

(defun compile-cdr (args ctx)
  (cdr (ctx-lookup (car args) ctx)))

(defun compile-null (args ctx)
  (null (gethash (car args) ctx)))

(defun compile-aref-macro (args ctx)
  (declare (ignore args))
  (declare (ignore ctx)))

(defun compile-if-macro (args ctx)
  ;; (format t "~a~%" args)
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
     (let ((res)
           (arg0 (ctx-lookup (car args) ctx))
           (arg1 (ctx-lookup (cadr args) ctx))
           (arg2+ (cddr args)))
       (if (null arg1)
           (setf res arg0)
           (progn
             (setf res (mod arg0 arg1))
             (do ((cur arg2+ (cdr cur)))
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
     (let ((res t)
           (arg0 (ctx-lookup (car args) ctx))
           (arg1 (ctx-lookup (cadr args) ctx))
           (arg2+ (cddr args)))
       (setf res (and arg0 arg1))
       (do ((cur arg2+ (cdr cur)))
           ((or (not cur) (not res)))
         (setf res (and res (ctx-lookup (car cur) ctx))))
       res))
    ((string= op "||")
     (let ((res nil)
           (arg0 (ctx-lookup (car args) ctx))
           (arg1 (ctx-lookup (cadr args) ctx))
           (arg2+ (cddr args)))
       (setf res (or arg0 arg1))
       (do ((cur arg2+ (cdr cur)))
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
       ;; (format t "a0: ~a, a1: ~a~%" arg0 arg1)
       (string= (format nil "~a" arg0) (format nil "~a" arg1))))))

(defun compile-let-macro (args ctx)
  (let ((res nil) (tmp-bindings nil))
    (destructuring-bind (bindings &rest body) args
      (do ((bcur bindings (cdr bcur)))
          ((not bcur))
        (setf tmp-bindings (cons (list (caar bcur) (gethash (caar bcur) ctx)) tmp-bindings))
        (setf (gethash (caar bcur) ctx) (ctx-lookup (cadar bcur) ctx)))
      (loop for bodyform in body do
            (setf res (compile-macro bodyform ctx)))
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
  (declare (ignore ctx))
  (let ((prefix (car args)))
    (if (null prefix) (setf prefix "_G"))
    (make-symbol (concatenate 'string prefix
                              (format nil "_~a_" (get-universal-time))
                              (format nil "~a" (incf *eclisp-gensym-counter*))))))

(defun compile-list-macro (args ctx)
  (if (null args) '()
      (cons (ctx-lookup (car args) ctx) (compile-list-macro (cdr args) ctx))))

(defun compile-append (args ctx)
  (apply #'append (mapcar #'(lambda (x) (ctx-lookup x ctx)) args)))

(defun compile-macro (form ctx)
  (let ((res nil))
    (setf res
          (if (consp form)
              (destructuring-bind (op &rest args) form
                (cond
                  ((string= "list"      (string op)) (compile-list-macro args ctx))
                  ((string= "append"    (string op)) (compile-append     args ctx))
                  ((string= "backquote" (string op)) (compile-backquote  args ctx))
                  ((string= "quote"     (string op)) (compile-quote      args ctx))
                  ((string= "symbolp"   (string op)) (compile-symbolp    args ctx))
                  ((string= "listp"     (string op)) (compile-listp      args ctx))
                  ((string= "car"       (string op)) (compile-car        args ctx))
                  ((string= "cdr"       (string op)) (compile-cdr        args ctx))
                  ((string= "null"      (string op)) (compile-null       args ctx))
                  ((string= "consp"     (string op)) (compile-consp      args ctx))
                  ((string= "stringp"   (string op)) (compile-stringp    args ctx))
                  ((string= "length"    (string op)) (compile-length     args ctx))
                  ((string= "aref"      (string op)) (compile-aref-macro args ctx))
                  ((string= "numberp"   (string op)) (compile-numberp    args ctx))
                  ((string= "concat"    (string op)) (compile-concat     args ctx))
                  ((string= "if"        (string op)) (compile-if-macro   args ctx))
                  ((string= "gensym"    (string op)) (compile-gensym     args ctx))
                  ((string= "let"       (string op)) (compile-let-macro  args ctx))
                  ((member (string op) '("<" ">" "<=" ">=" ">" "==" "!=" "&&" "||") :test #'equal)
                   (compile-op-macro op args ctx))
                  ;; not implemented ^ | & ~ << >>
                  ((member (string op) '("+" "-" "*" "/" "%" "^" "|" "&" "~" "<<" ">>")
                           :test #'equal)
                   (compile-op-macro op args ctx))
                  (t (if (gethash op macrofn-tbl)
                         (eval-macrofn op args ctx)
                         (error (format nil "call to a C function (here, ~a) through the ffi is not yet unsupported.~%" op))))))
              (format t "unsupported: ~a~%" form)))
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
  (let ((name nil) (macrofn-args nil) (documentation nil) (body nil))
    (setf name (car args))
    (setf macrofn-args (cadr args))
    (if (stringp (caddr args))
        (progn
          (setf documentation (caddr args))
          (setf body (cadddr args)))
        (setf body (caddr args)))
    (setf (gethash name macrofn-tbl) (list macrofn-args body))))

(defun expand-macro-args (args tmpl ctx)
  (when args
    (if (listp (car tmpl))
        (progn
          (expand-macro-args (car args) (car tmpl) ctx)
          (expand-macro-args (cdr args) (cdr tmpl) ctx))
        (if (string= (string (car tmpl)) "&body")
            (setf (gethash (cadr tmpl) ctx) args)
            (progn
              (setf (gethash (car tmpl) ctx) (car args))
              (expand-macro-args (cdr args) (cdr tmpl) ctx))))))

(defun expand-macro (macro args)
  (let ((ctx (make-hash-table))
        (tmpl (car (gethash macro macro-tbl)))
        (macro-body (cadr (gethash macro macro-tbl))))
    (expand-macro-args args tmpl ctx)
    (compile-macro macro-body ctx)))

(defun expand-macrofn-args (args tmpl ctx ctx-ref)
  (when args
    (if (listp (car tmpl))
        (progn
          (expand-macrofn-args (car args) (car tmpl) ctx ctx-ref)
          (expand-macrofn-args (cdr args) (cdr tmpl) ctx ctx-ref))
        (if (string= (string (car tmpl)) "&body")
            ;; we should clarify where the evaluation of macrofn takes place
            ;; because this may raise issues in the $body case.
            (setf (gethash (cadr tmpl) ctx) (ctx-lookup (car args) ctx-ref))
            (progn
              (setf (gethash (car tmpl) ctx) (ctx-lookup (car args) ctx-ref))
              (expand-macrofn-args (cdr args) (cdr tmpl) ctx ctx-ref))))))

(defun eval-macrofn (fn args ctx-ref)
  (let ((ctx (make-hash-table))
        (tmpl (car (gethash fn macrofn-tbl)))
        (fn-body (cadr (gethash fn macrofn-tbl))))
    (expand-macrofn-args args tmpl ctx ctx-ref)
    (compile-macro fn-body ctx)))

(defun compile-macrolet (args)
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
      (setf res
            (list* '|progn|
                   (loop for bodyform in body collect (compile-form bodyform))))
      ;; remove the bindings
      (do ((bcur tmp-bindings (cdr bcur)))
          ((not bcur))
        (remhash (caar bcur) macro-tbl)
        (when (cadar bcur)
          (setf (gethash (caar bcur) macro-tbl) (cadar bcur)))))
    res))

(defvar kwd-behavior
  '(("#include"  . (compile-call     . print-cpp-include))
    ("#define"   . (compile-call     . print-cpp-define))
    ("#if"       . (compile-cpp-if   . print-cpp-if))
    ("%type"     . (compile-call     . print-as-type))
    ("%:"        . (compile-verbatim . print-verbatim))
    ("%comment"  . (compile-call     . print-comment))
    ("break"     . (compile-call     . print-break))
    ("continue"  . (compile-call     . print-continue))
    ("%funcall"  . (compile-call     . print-funcall))
    ("cast"      . (compile-call     . print-cast))
    ("def"       . (compile-def      . print-def))
    ("seq"       . (compile-call     . print-seq))
    ("list"      . (compile-call     . print-list))
    ("progn"     . (compile-call     . print-progn))
    ("addr"      . (compile-call     . print-addr))
    ("deref"     . (compile-call     . print-deref))
    ("."         . (compile-call     . print-dot))
    ("->"        . (compile-call     . print-arrow))
    ("aref"      . (compile-call     . print-aref))
    ("if"        . (compile-call     . print-if))
    ("label"     . (compile-call     . print-label))
    ("goto"      . (compile-call     . print-goto))
    ("?:"        . (compile-call     . print-ite))
    ("for"       . (compile-call     . print-for))
    ("do-while"  . (compile-call     . print-do-while))
    ("while"     . (compile-call     . print-while))
    ("switch"    . (compile-switch   . print-switch))
    ("return"    . (compile-call     . print-return))
    ("quote"     . (compile-quote-c  . print-quote))
    ("backquote" . (compile-backquote-c  . print-backquote))
    ("="         . (compile-call     . print-set))
    ("+="        . (compile-call     . print-set))
    ("-="        . (compile-call     . print-set))
    ("*="        . (compile-call     . print-set))
    ("/="        . (compile-call     . print-set))
    ("%="        . (compile-call     . print-set))
    ("&="        . (compile-call     . print-set))
    ("^="        . (compile-call     . print-set))
    ("|="        . (compile-call     . print-set))
    ("<<="       . (compile-call     . print-set))
    (">>="       . (compile-call     . print-set))
    ("<"         . (compile-call     . print-cmp-op))
    (">"         . (compile-call     . print-cmp-op))
    ("<="        . (compile-call     . print-cmp-op))
    (">="        . (compile-call     . print-cmp-op))
    ("=="        . (compile-call     . print-cmp-op))
    ("!="        . (compile-call     . print-cmp-op))
    ("&&"        . (compile-call     . print-cmp-op))
    ("||"        . (compile-call     . print-cmp-op))
    ("+"         . (compile-call     . print-arith-binop))
    ("-"         . (compile-call     . print-arith-binop))
    ("*"         . (compile-call     . print-arith-binop))
    ("/"         . (compile-call     . print-arith-binop))
    ("%"         . (compile-call     . print-arith-binop))
    ("^"         . (compile-call     . print-arith-binop))
    ("|"         . (compile-call     . print-arith-binop))
    ("&"         . (compile-call     . print-arith-binop))
    ("~"         . (compile-call     . print-arith-binop))
    ("!"         . (compile-call     . print-arith-binop))
    ("<<"        . (compile-call     . print-arith-binop))
    (">>"        . (compile-call     . print-arith-binop))
    ("macro"     . (register-macro   . nil))
    ("macrofn"   . (register-macrofn . nil))
    ("macrolet"  . (compile-macrolet . nil))))

(defun compile-call (form)
  (list* (car form) (mapcar (lambda (x) (compile-form x)) (cdr form))))

(defun compile-form (form)
  "Compile an eclisp FROM and write it on TO-STREAM"
  (if (consp form)
      (destructuring-bind (op &rest args) form
        (when (gethash op macro-tbl)
          (setf form (expand-macro op args))))
      (when (gethash form macro-tbl)
        (setf form (expand-macro form nil))))
  (if (consp form)
      (destructuring-bind (op &rest args) form
        (let ((fn (cadr (assoc (string op) kwd-behavior :test #'string=))))
          (if fn
              (cond ((eql fn 'compile-call)
                     (funcall fn form))
                    ((or (member fn '(register-macro register-macrofn)))
                     (funcall fn args)
                     nil)
                    ((eql fn 'compile-macrolet) (funcall fn args))
                    ((eql fn 'compile-quote-c)
                     (let ((res (funcall fn args)))
                       (if (listp res) `(|quote| ,res) res)))
                    ((eql fn 'compile-backquote-c)
                     (let ((res (funcall fn args)))
                       (if (listp res) `(|backquote| ,res) res)))
                    (t
                     (list* op (funcall fn args))))
              (compile-call form))))
      form))

(defun print-form (form stmtp indent to-stream)
  "Compile an eclisp FROM and write it on TO-STREAM"
  (loop
   (if (consp form)
       (destructuring-bind (op &rest args) form
         (if (gethash op macro-tbl)
             (setf form (expand-macro op args))
             (return)))
       (if (gethash form macro-tbl)
           (setf form (expand-macro form nil))
           (return))))
  (if (consp form)
      (destructuring-bind (op &rest args) form
        (let ((fn (cddr (assoc (string op) kwd-behavior :test #'string=))))
          (unless (eql fn 'print-progn)
            (setf indent (max 0 indent)))
          (if fn
              (funcall fn form stmtp indent to-stream)
              (progn
                (format to-stream "~v@{~C~:*~}" indent #\Space)
                (format to-stream "~a (~{~a~^, ~})"
                        (car form)
                        (mapcar (lambda (x) (if (stringp x) (format nil "\"~a\"" x)
                                                (with-output-to-string (s) (print-form x nil 0 s))))
                                (cdr form)))
                (when stmtp (format to-stream ";~%"))))))
      (progn
        (unless (eql form nil)
          (format to-stream "~v@{~C~:*~}" indent #\Space)
          (if (stringp form)
              (format to-stream "\"~a\"" form)
              (format to-stream "~a" form))))))

(defun print-eclisp (from-stream to-stream)
  "Write the result of the compilation of the content of FROM-STREAM into
TO-STREAM."
  (loop for form = (parse from-stream)
        while form do
        (print-form (compile-form form) t -1 to-stream)))

(defun main ()
  "The entry point."
  (print-eclisp *standard-input* *standard-output*))
