;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

(in-package :eclisp)

(defclass eclisp-symbol ()
  ((name               :initform (error "name: mandatory argument.")
                       :initarg :name
                       :accessor es-name
                       :documentation
    "The name of the symbol")
   (attributes         :initform (make-hash-table :test 'equal)
                       :initarg :attributes
                       :accessor es-attrs
                       :documentation
    "The attributes of the symbols stored as a hash-table.")
   (value              :initarg :value
                       :initform nil
                       :accessor es-value
                       :documentation
    "The actual content to which the symbol is bound.")
   (type               :initarg :type
                       :initform nil
                       :accessor es-Type
                       :documentation
    "The symbol type: variable, function, macro, define, tag or typedef.")
   (documentation      :initarg :documentation
                       :initform nil
                       :accessor es-doc
                       :documentation
    "The docstring attached to the symbol.")))

(defmethod print-object ((obj eclisp-symbol) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((name es-name) (attrs es-attrs) (type es-type)
                     (val es-value) (doc es-doc))
        obj
      (format stream "name: ~a, attrs: {~{~{(~a : ~a)~}~^~}},~@
                      value: ~a, type: ~a, doc: ~a~%"
              name
              (loop for key being the hash-keys of attrs
                      using (hash-value value)
                    collect (list key value))
              val type doc))))

(defvar macro-tbl (make-hash-table)
  "A global variable with the currently defined macros.")

(defvar symbol-macro-tbl (make-hash-table)
  "A global variable with the currently defined symbol macros.")

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
                (cond ((eq (et-type filename) :eclisp-symbol) "#include ~a~%")
                      (t "#include \"~a\"~%"))
                (et-value filename))))

(defun eclisp-file-p (filename)
  (let* ((filename (string filename))
         (filename-len (length filename))
         (pos (search ".eclisp" filename :start2 (max 0 (- filename-len 9)))))
    (and pos (or (= pos (- filename-len 7))
                 (and (= pos (- filename-len 8))
                      (char= (aref filename (1- filename-len)) #\>)
                      (char= (aref filename 0) #\<))
                 (and (= pos (- filename-len 8))
                      (char= (aref filename (1- filename-len)) #\h))
                 (and (= pos (- filename-len 9))
                      (char= (aref filename (1- filename-len)) #\>)
                      (char= (aref filename 0) #\<))))))

(defun convert-file-extension (filename)
  (let* ((orig filename)
         (sym-p (symbolp filename))
         (filename (string filename))
         (filename-len (length filename))
         (pos (search ".eclisp" filename :start2 (max 0 (- filename-len 9))))
         (pos-h (search ".eclisph" filename :start2 (max 0 (- filename-len 9)))))
    (cond (pos-h
           (let ((tmp (concatenate 'string (subseq filename 0 pos) ".h" (subseq filename (+ pos 8)))))
             (if sym-p (intern tmp) tmp)))
          (pos
           (let ((tmp (concatenate 'string (subseq filename 0 pos) ".c" (subseq filename (+ pos 7)))))
             (if sym-p (intern tmp) tmp)))
          (t orig))))

(defun read-macro-definitions (filename &aux (local-macros (make-hash-table :test 'equal)))
  (with-open-file (f filename)
    (loop for form = (eclisp-read f)
          while form do
          (when (or (eq (car form) +eclisp-macro+)
                    (eq (car form) +eclisp-macrofn+)
                    (eq (car form) +eclisp-macrolet+)
                    (gethash (et-value (car form)) local-macros))
            (setf (gethash (et-value (cadr form)) local-macros) 1)
            (eclisp-eval form nil)))))

(defun print-cpp-define (form stmtp indent to-stream)
  "Compile a define directive: %(define name substitution) and write it
to TO-STREAM. FORM is a cons made of NAME and SUBSTITUTION.
NAME and SUBSTITUTION are both strings and should enclosed in double-quotes.
NAME can contain parenthesis just like a C macro does, i.e., no spaces before
the parenthesis.  SUBSTITUTION should be valid C code."
  (declare (ignore stmtp))
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "#define ~{~a~^ ~}~%"
          (mapcar #'(lambda (x)
                      (cond
                        ((eq (et-type x) :eclisp-string) (format nil "\"~a\"" (et-value x)))
                        ((eq (et-type x) :eclisp-character) (format nil "\'~a\'" (et-value x)))
                        (t (et-value x)))) form)))

(defun print-verbatim (form stmtp indent to-stream)
  (declare (ignore indent))
  (pop form)
  (format to-stream "~a" (car form))
  (when stmtp (format to-stream "~%")))

(defun print-cmp-op (form stmtp indent to-stream)
  "Compile a FORM beginning with a compare operator (< > <= >= > ==) into a valid
C expression and write it on TO-STREAM.
The CPP parameter enable C Preprocessor mode, restricting what can appear in
the expression."
  (declare (ignore stmtp))
    (destructuring-bind (op &rest args) form
      (format to-stream "(")
      (do ((cur args (cdr cur)))
          ((not (cdr cur)))
        (format to-stream "(")
        (print-form (car cur) nil indent to-stream)
        (format to-stream " ~a " (et-value op))
        (print-form (cadr cur) nil indent to-stream)
        (format to-stream ")")
        (when (cddr cur)
          (format to-stream " && ")))
      (format to-stream ")")))

(defun print-cpp-cond-expr (form stmtp indent to-stream)
  "Compile a preprocessor condition FORM, i.e.,  one that may appear after
#if and #elif, and write it on TO-STREAM."
  (if (consp form)
      (destructuring-bind (op &rest args) form
        (cond
          ((string= "defined" (et-value op))
           (format to-stream "defined (~a)" (et-value (car args))))
          ((member (et-value op) '("<" ">" "<=" ">=" ">" "==") :test #'equal)
           (print-cmp-op form stmtp indent to-stream))
          ((member (et-value op) '("!" "+" "-" "*" "/" "%" "^" "|" "||" "&" "&&" "~" "<<" ">>")
                   :test #'equal)
           (print-binop form stmtp 0 to-stream))))
      (format to-stream "~a" (et-value form))))

(defun print-cond-expr (form stmtp indent to-stream)
  "Compile a condition FORM, i.e.,  one that may appear after
and if, and write it on TO-STREAM."
  (if (consp form)
      (destructuring-bind (op &rest args) form
        (declare (ignore args))
        (cond
          ((member (et-value op) '("<" ">" "<=" ">=" ">" "==" "!=") :test #'equal)
           (print-cmp-op form stmtp indent to-stream))
          ((member (et-value op) '("+" "-" "*" "/" "%" "^" "|" "||" "&" "&&" "~" "<<" ">>")
                   :test #'equal)
           (print-binop form stmtp 0 to-stream))))
      (format to-stream "~a" (et-value form))))

(defun print-cpp-if (form stmtp indent to-stream)
  "Compile a if preprocessor directive FORM and write it on TO-STREAM"
  (declare (ignore stmtp))
  (pop form)
  (macrolet ((otherwise_p (v) `(and (eq (et-type ,v) :eclisp-symbol)
                                    (or (string= (et-value ,v) "t")
                                        (string= (et-value ,v) "otherwise")))))
    (do ((cur form (cdr cur))
         (first t nil))
        ((not cur))
      (destructuring-bind (cond &rest body) (car cur)
        (format to-stream "~v@{~C~:*~}" indent #\Space)
        (if first
            (format to-stream "#if ")
            (format to-stream (if (otherwise_p cond)
                                  "~v@{~C~:*~}#else" "~v@{~C~:*~}#elif ")
                    indent #\Space))
        (unless (otherwise_p cond) (print-cpp-cond-expr cond nil 0 to-stream))
        (format to-stream "~%")
        (loop for b in body do
          (print-form b t (+ 2 indent) to-stream))))
    (format to-stream "~v@{~C~:*~}#endif~%" indent #\Space)))

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

(defun group-bitfield (lst)
  (unless (null lst)
    (if (eq (et-type (cadr lst)) :eclisp-symbol)
        (cons (list (car lst)) (group-bitfield (cdr lst)))
        (cons (list (car lst) (cadr lst)) (group-bitfield (cddr lst))))))

(defvar +array-kwd+ "array"
  "The keyword used to declare arrays.")
(defvar +ptr-kwd+ "ptr"
  "The keyword used to declare pointers.")
(defvar +fun-kwd+ "->"
  "The keyword to declare functions.")

(defun print-type (name type to-stream)
  "Compile TYPE and write it on TO-STREAM."
  (print-ll (print-c-type name type nil) 0 to-stream))

(defun print-as-type (form stmtp indent to-stream)
  (when stmtp
    (format to-stream "~v@{~C~:*~}{~%" indent #\Space))
  (format to-stream "~a" (with-output-to-string (s) (print-type "" (cadr form) s)))
  (when stmtp
    (format to-stream ";~%")))

(defun print-prog (form stmtp indent to-stream)
  (declare (ignore stmtp))
  (pop form)
  (cond ((>= indent 0)
         (format to-stream "~v@{~C~:*~}{~%" indent #\Space)
         (loop for f in form do (print-form f t (+ 2 indent) to-stream))
         (format to-stream "~v@{~C~:*~}}~%" indent #\Space))
        (t (loop for f in form do (print-form f t indent to-stream)))))

(defun print-prog* (form stmtp indent to-stream)
  (declare (ignore stmtp))
  (pop form)
  (loop for f in form do (print-form f t indent to-stream)))

(defun print-funcall (form stmtp indent to-stream)
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "~a (~{~a~^, ~})"
          (et-value (car form))
          (mapcar (lambda (x) (if (stringp x) (format nil "\"~a\"" x)
                                  (with-output-to-string (s) (print-form x nil 0 s))))
                  (cdr form)))
  (when stmtp (format to-stream ";~%")))

;; (asm "..." "..." "..."
;;      :in (("r" sym) ("+r" sym))
;;      :out (("=r" sym) ...)
;;      :clobbers (...)
;;      :volatile)

(defun print-asm (form stmtp indent to-stream)
  (pop form)
  (let (asm-forms ins outs clobbers volatile (suffix "") raw)
    (do ((cur form (cdr cur)))
        ((not cur))
        (macrolet
          ((process-asm-decls (section decls cur)
              `(when (string= (et-value (car cur)) ,section)
                 (if (and (cadr ,cur) (listp (cadr ,cur)))
                   (progn
                     (if (listp (caadr ,cur))
                       (loop for (constraint sym) in (cadr ,cur) do
                             (push (cons (et-value constraint) (et-value sym)) ,decls))
                       (push (cons (et-value (caadr ,cur)) (et-value (cadadr ,cur))) ,decls)))
                   (error (format t "empty ~a section" ,section)))
                 (setf ,cur (cdr ,cur))))
            (process-clobbers (section decls cur)
              `(when (string= (et-value (car ,cur)) ,section)
                 (if (cadr ,cur)
                   (if (listp (cadr ,cur))
                       (loop for clobber in (cadr ,cur) do
                             (push (et-value clobber) ,decls))
                       (push (et-value (cadr ,cur)) ,decls))
                   (error (format t "empty ~a section" ,section)))
                 (setf ,cur (cdr ,cur)))))
          (cond
            ((eq (et-type (car cur)) :eclisp-string)
             (push (et-value (car cur)) asm-forms))
            ((eq (et-type (car cur)) :eclisp-symbol)
             (process-asm-decls ":in" ins cur)
             (process-asm-decls ":out" outs cur)
             (process-clobbers ":clobber" clobbers cur)
             (when (string= (et-value (car cur)) ":volatile")
               (cond
                 ((and (cadr cur) (string= (et-value (cadr cur)) "nil"))
                  (setf volatile nil)
                  (setf cur (cdr cur)))
                 ((and (cadr cur) (string= (et-value (cadr cur)) "t"))
                  (setf volatile t)
                  (setf cur (cdr cur)))
                 (t (setf volatile t))))
             (when (string= (et-value (car cur)) ":raw")
               (cond
                 ((and (cadr cur) (string= (et-value (cadr cur)) "nil"))
                  (setf raw nil)
                  (setf cur (cdr cur)))
                 ((and (cadr cur) (string= (et-value (cadr cur)) "t"))
                  (setf raw t)
                  (setf cur (cdr cur)))
                 (t (setf raw t))))
             (when (string= (et-value (car cur)) ":suffix")
               (if (and (cadr cur) (eq (et-type (cadr cur)) :eclisp-string))
                 (progn
                   (setf suffix (et-value (cadr cur)))
                   (setf cur (cdr cur)))
                 (error "emtpy suffix")))))))
    (format to-stream "~v@{~C~:*~}" indent #\Space)
    (format to-stream "__asm__ ")
    (when volatile (format to-stream "volatile "))
    (format to-stream "(~%")
    (do ((cur (reverse asm-forms) (cdr cur)))
        ((not cur))
          (format to-stream "~v@{~C~:*~}" (+ 2 indent) #\Space)
          (format to-stream "\"~a\"" (concatenate 'string (car cur) suffix))
          (when (cdr cur) (format to-stream "~%")))

    (unless raw
      (format to-stream "~%~v@{~C~:*~}:" (+ 2 indent) #\Space)
      (do ((cur outs (cdr cur)))
          ((not cur))
            (format to-stream " ~s (~a)" (caar cur) (cdar cur))
            (when (cdr cur) (format to-stream ",")))
      (format to-stream "~%~v@{~C~:*~}:" (+ 2 indent) #\Space)
      (do ((cur (reverse ins) (cdr cur)))
          ((not cur))
            (format to-stream " ~s (~a)" (caar cur) (cdar cur))
            (when (cdr cur) (format to-stream ",")))
      (format to-stream "~%~v@{~C~:*~}:" (+ 2 indent) #\Space)
      (do ((cur clobbers (cdr cur)))
          ((not cur))
            (format to-stream " ~s" (car cur))
            (when (cdr cur) (format to-stream ","))))
    (format to-stream "~%~v@{~C~:*~})" indent #\Space)
    (when stmtp (format to-stream ";~%"))))

(defun print-defvar (form stmtp indent to-stream)
  "Compile a form which declares a global variable.
                  (def var type value documentation)"
  (destructuring-bind (var type value documentation) form
    (when (and (symbolp value) (string= (string value) "%nothing")) (setf value nil))
    (unless (null documentation)
      (progn
        (format to-stream "~v@{~C~:*~}" indent #\Space)
        (format to-stream "/* ~a  */~%"
                (regex-replace-all "\\n" (et-value documentation)
                                   (concatenate 'string '(#\Newline) "   "
                                                (format nil "~v@{~C~:*~}" indent #\Space))))))
    (when var
      (when (string= (gethash "linkage" (es-attrs var)) "static")
        (setf type `(,(intern-eclisp-token "static") ,type)))
      (when (string= (gethash "linkage" (es-attrs var)) "extern")
        (setf type `(,(intern-eclisp-token "extern") ,type))))
    (format to-stream "~v@{~C~:*~}" indent #\Space)
    (print-type (when var (es-name var)) type to-stream)
    (when (and (es-attrs var)
               (or (multiple-value-bind (var present-p) (gethash "packed" (es-attrs var)) present-p)
                   (multiple-value-bind (var present-p) (gethash "aligned" (es-attrs var)) present-p)
                   (multiple-value-bind (var present-p) (gethash "section" (es-attrs var)) present-p)))
      (format to-stream " __attribute__ ((")
      (let ((comma-p nil))
      (when (and (multiple-value-bind (var present-p) (gethash "packed" (es-attrs var)) present-p)
                 (or (null (gethash "packed" (es-attrs var)))
                     (string= (gethash "packed" (es-attrs var)) "t")))
          (if comma-p (format to-stream ", "))
          (format to-stream "packed")
          (setf comma-p t))
      (when (gethash "section" (es-attrs var))
        (format to-stream "section (\"~a\")" (gethash "section" (es-attrs var)))
        (setf comma-p t))
      (when (multiple-value-bind (var present-p) (gethash "aligned" (es-attrs var)) present-p)
          (if comma-p (format to-stream ", "))
          (format to-stream "aligned (~a)" (gethash "aligned" (es-attrs var)))
          (setf comma-p t))
      (format to-stream "))")))
    (when value
      (format to-stream " = ~a"
              (with-output-to-string (s) (print-form (eclisp-eval value nil) nil 0 s))))
      (when stmtp (format to-stream ";~%"))))

(defun compile-symbol-definition (symdef)
  "Reads a name and its attributes.
All this information is returned as an ECLISP-SYMBOL instance"
  (if (listp symdef)
      (let* ((sym (make-instance 'eclisp-symbol :name (car symdef)))
             (cur-key nil))
        (loop for elt in (cdr symdef) do
              (cond ((and (or (stringp elt) (symbolp elt))
                          (char= (aref (string elt) 0) #\:))
                 (setf cur-key (subseq (string elt) 1))
                 (setf (gethash cur-key (es-attrs sym)) nil))
                (t
                 (if (null cur-key)
                     (error "cur-key is nil")
                     (setf (gethash cur-key (es-attrs sym)) elt)))))
        sym)
      (make-instance 'eclisp-symbol :name symdef)))

(defun print-defun (form stmtp indent to-stream)
  "Compile a form which declares a global variable.
                  (def f (-> rettype params) documentation body)
BODY is optional. DOCUMENTATION is optional"
  (destructuring-bind (var type documentation body) form
    (unless (null documentation)
      (progn
        (format to-stream "~v@{~C~:*~}" indent #\Space)
        (format to-stream "/* ~a  */~%"
                (regex-replace-all "\\n" (et-value documentation)
                                   (concatenate 'string '(#\Newline) "   "
                                                (format nil "~v@{~C~:*~}" indent #\Space))))))
    (format to-stream "~v@{~C~:*~}" indent #\Space)
    (when var
      (when (multiple-value-bind (val present-p) (gethash "inline" (es-attrs var))
              (and (not (string= val "never")) present-p))
        (setf type `(,(intern-eclisp-token "->") (,(intern-eclisp-token "inline") ,(cadr type)) ,@(cddr type))))
      (when (string= (gethash "linkage" (es-attrs var)) "static")
        (setf type `(,(intern-eclisp-token "->") (,(intern-eclisp-token "static") ,(cadr type)) ,@(cddr type)))))
    (when (or (gethash "section" (es-attrs var))
              (gethash "inline" (es-attrs var))
              (multiple-value-bind (val present-p) (gethash "noreturn" (es-attrs var)) present-p)
              (multiple-value-bind (val present-p) (gethash "interrupt" (es-attrs var)) present-p))
      (format to-stream "__attribute__ ((")
      (let ((comma-p nil))
        (when (gethash "section" (es-attrs var))
          (format to-stream "section (\"~a\")" (gethash "section" (es-attrs var)))
          (setf comma-p t))
        (when (string= (gethash "inline" (es-attrs var)) "always")
          (when comma-p (format to-stream ", "))
          (format to-stream "always_inline")
          (setf comma-p t))
        (when (string= (gethash "inline" (es-attrs var)) "never")
          (when comma-p (format to-stream ", "))
          (format to-stream "noinline")
          (setf comma-p t))
        (when (multiple-value-bind (val present-p) (gethash "noreturn" (es-attrs var)) present-p)
          (when comma-p (format to-stream ", "))
          (format to-stream "noreturn")
          (setf comma-p t))
        (when (multiple-value-bind (val present-p) (gethash "interrupt" (es-attrs var)) present-p)
          (when comma-p (format to-stream ", "))
          (format to-stream "interrupt")
          (setf comma-p t))
      (format to-stream "))~%")))
    (print-type (when var (es-name var)) type to-stream)
    (when body
      (format to-stream "~%")
      (print-prog (list* '|prog| body) stmtp indent to-stream))
    (when (not body)
      (format to-stream ";~%"))))

(defun print-def (form stmtp indent to-stream)
  (pop form)
  (let ((type nil))
    (if (consp (car form))
        (setf type (car form))
        ;; name is present
        (progn
          (when (consp (cadr form)) (setf type (cadr form)))))
    (cond ((and type (eq (car type) +eclisp-arrow+))
           (print-defun form stmtp indent to-stream))
          (t (print-defvar form stmtp indent to-stream)))))

(defun print-if (form stmtp indent to-stream)
  (declare (ignore stmtp))
  (pop form)
  (format to-stream "~v@{~C~:*~}if (" indent #\Space)
  (print-form (car form) nil 0 to-stream)
  (format to-stream ")~%")
  (print-form (cadr form) t (+ 2 indent) to-stream)
  (unless (null (caddr form))
    (format to-stream "~v@{~C~:*~}else ~%" indent #\Space)
    (print-form (caddr form) t (+ 2 indent) to-stream)))

(defun print-binop (form stmtp indent to-stream)
  (destructuring-bind (op &rest args) form
    (cond
      (stmtp (format to-stream "~v@{~C~:*~}" indent #\Space))
      ((not stmtp) (format to-stream "(")))
    (if (cdr args)
        (let ((cur args))
             (loop while cur do
                   (print-form (car cur) nil 0 to-stream)
                   (if (cdr cur) (format to-stream " ~a " (et-value op)))
                   (setf cur (cdr cur))))
        (progn
          (format to-stream "(~a " (et-value op))
          (print-form (car args) nil 0 to-stream)
          (format to-stream ")")))
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
      ;; fixme: unquote self-quoting symbols
      (loop while (and (listp cur)
                       (listp (car cur))
                       (eq (caar cur) +eclisp-quote+)
                       (= 1 (length (cdar cur))))
            do (setf cur (cons (cadar cur) (cdr cur))))
      (cond
        ((and (consp (car cur))
              (eq (et-type (caar cur)) :eclisp-symbol)
              (string= (caar (et-value cur)) ":"))
         (if (string= (et-value cur) ":")
           (progn
             (format to-stream "[~a] = "
                     (with-output-to-string (s) (print-form (cadadr cur) nil 0 s)))
             (setf cur (cddr cur)))
           (progn
             (format to-stream "[~a] = "
                     (with-output-to-string (s) (print-form (cadar cur) nil 0 s)))
            (setf cur (cdr cur))))
         )
        ((and
          (eq (et-type (car cur)) :eclisp-symbol)
          (char= (aref (et-value (car cur)) 0) #\.))
         (format to-stream "~a = " (et-value (car cur)))
         (pop cur))
        ((and
          (eq (et-type (car cur)) :eclisp-symbol)
          (char= (aref (et-value (car cur)) 0) #\:))
         (if (string= (et-value (car cur)) ":")
           (progn
             (format to-stream "[~a] = "
                     (with-output-to-string (s) (print-form (cadr cur) nil 0 s)))
             (pop cur)
             (pop cur))
           (progn
             (format to-stream "[~a] = " (subseq (et-value (car cur)) 1))
             (pop cur)))))
      (print-form (car cur) nil 0 to-stream)
      (if (cdr cur) (format to-stream ", "))
      (setf cur (cdr cur))))
  (format to-stream "}")
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
  (declare (ignore stmtp))
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (print-form (car form) nil indent to-stream)
  (let ((cur (cdr form)))
    (loop while cur do
          (format to-stream "[")
          (print-form (car cur) nil 0 to-stream)
          (format to-stream "]")
          (setf cur (cdr cur)))))

(defun print-while (form stmtp indent to-stream)
  (declare (ignore stmtp))
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "while (")
  (print-form (car form) nil 0 to-stream)
  (format to-stream ")~%")
  (if (cadr form)
      (print-form `(,+eclisp-prog+ ,@(cdr form)) t (+ 2 indent) to-stream)
      (format to-stream ";~%")))

(defun print-do-while (form stmtp indent to-stream)
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "do")
  (format to-stream "~%")
  (print-form `(,+eclisp-prog+ ,@(cdr form)) t (+ 2 indent) to-stream)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "while (")
  (print-cond-expr (car form) stmtp 0 to-stream)
  (format to-stream ");~%"))

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
  (declare (ignore stmtp))
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
      (print-form `(,+eclisp-prog+ ,@(cdddr form)) t (+ 2 indent) to-stream)
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
  (declare (ignore stmtp))
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
                    (if (eq l (intern-eclisp-token "default"))
                        (format to-stream "~v@{~C~:*~}default:~%" (+ 2 indent) #\Space)
                        (progn
                          (format to-stream "~v@{~C~:*~}case " (+ 2 indent) #\Space)
                          (print-form l nil 0 to-stream)
                          (format to-stream ":~%"))))
              (progn
                (if (eq label (intern-eclisp-token "default"))
                    (format to-stream "~v@{~C~:*~}default:~%" (+ 2 indent) #\Space)
                    (progn
                      (format to-stream "~v@{~C~:*~}case " (+ 2 indent) #\Space)
                      (print-form label nil 0 to-stream)
                      (format to-stream ":~%")))))
          (print-form `(,+eclisp-prog+ ,@clauses) t (+ indent 4) to-stream)))
  (format to-stream "~v@{~C~:*~}  }~%" indent #\Space))

(defun print-label (form stmtp indent to-stream)
  (declare (ignore stmtp))
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "~a:~%" (et-value (car form))))

(defun print-goto (form stmtp indent to-stream)
  (declare (ignore stmtp))
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "goto ~a;~%" (et-value (car form))))

(defun print-comment (form stmtp indent to-stream)
  (declare (ignore stmtp))
  (pop form)
  (format to-stream "~v@{~C~:*~}" indent #\Space)
  (format to-stream "//~a~%" (et-value (car form))))

(defun compile-quote (args ctx)
  (declare (ignore ctx))
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
                (format to-stream "{")
                (print-quote-aux el nil (+ 2 indent) to-stream)
                (format to-stream "}")
                (when (cdr cur)
                  (format to-stream ", ")))
              (progn
                (if (eq (et-type el) :eclisp-symbol)
                    (progn
                      (cond
                        ((char= (aref (et-value el) 0) #\.)
                         (format to-stream "~a = " (et-value el)))
                        ((char= (aref (et-value el) 0) #\:)
                          (if (string= (et-value el) ":")
                            (progn
                              (format to-stream "[~a] = "
                                      (with-output-to-string (s) (print-form (cadr cur) nil 0 s)))
                              (setf cur (cdr cur)))
                                      (format to-stream "[~a] = " (subseq (et-value el) 1))))
                        (t
                         (format to-stream (cond ((eq (et-type args) :eclisp-string) "\"~a\"")
                                                 ((eq (et-type args) :eclisp-character) "'~a'")
                                                 (t "~a"))
                                 (et-value el))
                         (when (cdr cur)
                           (format to-stream ", ")))))
                    (progn
                      (format to-stream (cond ((eq (et-type el) :eclisp-string) "\"~a\"")
                                              ((eq (et-type el) :eclisp-character) "'~a'")
                                              (t "~a"))
                              (et-value el))
                      (when (cdr cur)
                        (format to-stream ", "))))))))
      (format to-stream (cond ((eq (et-type args) :eclisp-string) "\"~a\"")
                              ((eq (et-type args) :eclisp-character) "'~a'")
                              (t "~a"))
              (et-value args)))
  (when stmtp
    (format to-stream ";~%")))

(defun ctx-lookup (x ctx)
  (cond ((symbolp x) (gethash x ctx))
        ((listp x) (compile-macro x ctx))
        ((and (eq (type-of x) 'eclisp-token) (eq (et-type x) :eclisp-symbol)) (gethash x ctx))
        (t x)))

(defun compile-concat (args ctx)
  (intern-eclisp-token
    (apply #'concatenate 'string
           (mapcar #'(lambda (x) (format nil "~a" (et-value (ctx-lookup x ctx)))) args))
    :eclisp-string))

(defun compile-symbolicate (args ctx)
  (intern-eclisp-token
    (apply #'concatenate 'string
           (mapcar #'(lambda (x) (format nil "~a" (et-value (ctx-lookup x ctx)))) args))
    :eclisp-symbol))

(defun compile-car (args ctx)
  (car (ctx-lookup (car args) ctx)))

(defun compile-cdr (args ctx)
  (cdr (ctx-lookup (car args) ctx)))

(defun compile-null (args ctx)
  (null (gethash (car args) ctx)))

(defun compile-aref-macro (args ctx)
  (let ((seq (ctx-lookup (car args) ctx))
        (idx (ctx-lookup (cadr args) ctx)))
    (intern-eclisp-token
      (aref (et-value seq) (et-value idx))
      :eclisp-character)))

(defun compile-if-macro (args ctx)
  ;; (format t "~a ~a~%" (car args) (ctx-lookup (car args) ctx))
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
  (eq (et-type (ctx-lookup (car args) ctx))
      :eclisp-symbol))

(defun compile-numberp (args ctx)
  (eq (et-type (ctx-lookup (car args) ctx))
      :eclisp-num))

(defun compile-stringp (args ctx)
  (eq (et-type (ctx-lookup (car args) ctx))
      :eclisp-string))

(defun compile-length (args ctx)
  (length (et-value (ctx-lookup (car args) ctx))))

(defun compile-op-macro (op args ctx)
  (cond
    ((string= op "+")
     (let (res)
       (setf res (apply #'+ (mapcar #'(lambda (x) (et-value (ctx-lookup x ctx))) args)))
       (intern-eclisp-token
         (if (not (integerp res)) (coerce res 'float) res)
         :eclisp-number)))
    ((string= op "-")
     (let (res)
       (setf res (apply #'- (mapcar #'(lambda (x) (et-value (ctx-lookup x ctx))) args)))
       (intern-eclisp-token
         (if (not (integerp res)) (coerce res 'float) res)
         :eclisp-number)))
    ((string= op "*")
     (let (res)
       (setf res (apply #'* (mapcar #'(lambda (x) (et-value (ctx-lookup x ctx))) args)))
       (intern-eclisp-token
         (if (not (integerp res)) (coerce res 'float) res)
         :eclisp-number)))
    ((string= op "/")
     (let (res)
       (setf res (apply #'/ (mapcar #'(lambda (x) (et-value (ctx-lookup x ctx))) args)))
       (intern-eclisp-token
         (if (not (integerp res)) (coerce res 'float) res)
         :eclisp-number)))
    ((string= op "%")
     (let ((res)
           (arg0 (et-value (ctx-lookup (car args) ctx)))
           (arg1 (et-value (ctx-lookup (cadr args) ctx)))
           (arg2+ (cddr args)))
       (if (null arg1)
           (setf res arg0)
           (progn
             (setf res (mod arg0 arg1))
             (do ((cur arg2+ (cdr cur)))
                 ((not cur))
               (setf res (mod res (et-value (ctx-lookup (car cur) ctx)))))))
       (intern-eclisp-token
         (if (not (integerp res)) (coerce res 'float) res)
         :eclisp-number)))
    ;; todo: missing: ^ | & ~ << >>
    ((string= op "<")
     (apply #'< (mapcar #'(lambda (x) (et-value (ctx-lookup x ctx))) args)))
    ((string= op "<=")
     (apply #'<= (mapcar #'(lambda (x) (et-value (ctx-lookup x ctx))) args)))
    ((string= op ">")
     (apply #'> (mapcar #'(lambda (x) (et-value (ctx-lookup x ctx))) args)))
    ((string= op ">=")
     (apply #'> (mapcar #'(lambda (x) (et-value (ctx-lookup x ctx))) args)))
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
     (let ((arg0 (et-value (ctx-lookup (car args) ctx)))
           (arg1 (et-value (ctx-lookup (cadr args) ctx))))
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
      (loop for bodyform in body do
            (setf res (compile-macro bodyform ctx)))
      ;; remove the bindings
      (do ((bcur tmp-bindings (cdr bcur)))
          ((not bcur))
        (remhash (caar bcur) ctx)
        (when (cadar bcur)
          (setf (gethash (caar bcur) ctx) (cadar bcur)))))
    res))

(defun copy-hash-table (hash-table)
  (let ((ht (make-hash-table
             :test (hash-table-test hash-table)
             :rehash-size (hash-table-rehash-size hash-table)
             :rehash-threshold (hash-table-rehash-threshold hash-table)
             :size (hash-table-size hash-table))))
    (loop for key being each hash-key of hash-table
       using (hash-value value)
       do (setf (gethash key ht) value)
       finally (return ht))))

(defun compile-dbind-macro (args ctx)
  (let ((tmpl (car args))
        (arg (ctx-lookup (cadr args) ctx))
        (body (caddr args))
        (new-ctx (copy-hash-table ctx)))
    (expand-macro-args arg tmpl new-ctx)
    (compile-macro body new-ctx)))

(defvar *eclisp-gensym-counter* 0)

(defun compile-gensym (args ctx)
  ;; todo: make something more robust
  (declare (ignore ctx))
  (let ((prefix (car args)))
    (if (null prefix) (setf prefix "_G"))
    (intern-eclisp-token
     (concatenate 'string (et-value prefix)
                  (format nil "_~a_" (get-universal-time))
                  (format nil "~a" (incf *eclisp-gensym-counter*))))))

(defun compile-list-macro (args ctx)
  (if (null args) '()
      (cons (ctx-lookup (car args) ctx) (compile-list-macro (cdr args) ctx))))

(defun compile-append (args ctx)
  (apply #'append (mapcar #'(lambda (x) (ctx-lookup x ctx)) args)))

(defun compile-list* (args ctx)
  (apply #'list* (mapcar #'(lambda (x) (ctx-lookup x ctx)) args)))

(defun compile-macro (form ctx)
  (let ((res nil))
    (setf res
          (if (consp form)
              (destructuring-bind (op &rest args) form
                (cond
                  ((eq +eclisp-list+        op) (compile-list-macro  args ctx))
                  ((eq +eclisp-list*+       op) (compile-list*       args ctx))
                  ((eq +eclisp-append+      op) (compile-append      args ctx))
                  ((eq +eclisp-quote+       op) (compile-quote       args ctx))
                  ((eq +eclisp-symbolp+     op) (compile-symbolp     args ctx))
                  ((eq +eclisp-listp+       op) (compile-listp       args ctx))
                  ((eq +eclisp-car+         op) (compile-car         args ctx))
                  ((eq +eclisp-cdr+         op) (compile-cdr         args ctx))
                  ((eq +eclisp-null+        op) (compile-null        args ctx))
                  ((eq +eclisp-consp+       op) (compile-consp       args ctx))
                  ((eq +eclisp-stringp+     op) (compile-stringp     args ctx))
                  ((eq +eclisp-length+      op) (compile-length      args ctx))
                  ((eq +eclisp-aref+        op) (compile-aref-macro  args ctx))
                  ((eq +eclisp-numberp+     op) (compile-numberp     args ctx))
                  ((eq +eclisp-concat+      op) (compile-concat      args ctx))
                  ((eq +eclisp-symbolicate+ op) (compile-symbolicate args ctx))
                  ((eq +eclisp-if+          op) (compile-if-macro    args ctx))
                  ((eq +eclisp-gensym+      op) (compile-gensym      args ctx))
                  ((eq +eclisp-let+         op) (compile-let-macro   args ctx))
                  ((eq +eclisp-dbind+       op) (compile-dbind-macro args ctx))
                  ((member (et-value op) '("<" ">" "<=" ">=" ">" "==" "!=" "&&" "||") :test #'equal)
                    (compile-op-macro (et-value op) args ctx))
                   ;; not implemented ^ | & ~ << >>
                  ((member (et-value op) '("+" "-" "*" "/" "%" "^" "|" "&" "~" "<<" ">>")
                           :test #'equal)
                   (compile-op-macro (et-value op) args ctx))
                  (t (if (gethash op macrofn-tbl)
                         (eval-macrofn op args ctx)
                         (error (format nil "call to a C function (here, ~a) through the ffi is not yet unsupported.~%" op))))))
              (when form
                (format t "unsupported: ~a~%" form))))
    res))

(defun extract-keyword-args (args tmpl ctx)
  (labels ((group-kwd-val (args)
             (when args
               (cons (list (et-value (car args)) (cadr args))
                     (group-kwd-val (cddr args)))))
           (get-val (kwd)
             (cadr (assoc (if (char= (aref (et-value kwd) 0) #\:)
                              (et-value kwd)
                              (concatenate 'string ":" (et-value kwd)))
                          (group-kwd-val args)
                          :test #'string=)))
           (explode-kwd (kwd)
             (cond ((atom kwd) (list kwd kwd nil))
                   ((atom (car kwd)) (list (car kwd) (car kwd) (cadr kwd)))
                   ((and (consp (car kwd))
                         (atom (caar kwd)) (atom (cadar kwd)))
                    (list (caar kwd) (cadar kwd) (cadr kwd))))))
    (loop for kwd in tmpl
          for (accessor var-name default) = (explode-kwd kwd)
          for val = (if (get-val accessor) (get-val accessor) default) do
          (setf (gethash var-name ctx) val))))

(defun expand-macro-args (args tmpl ctx)
  (when (or args tmpl)
    (if (listp (car tmpl))
        (progn
          (expand-macro-args (car args) (car tmpl) ctx)
          (expand-macro-args (cdr args) (cdr tmpl) ctx))
      (cond ((or (string= (et-value (car tmpl)) "&body")
                 (string= (et-value (car tmpl)) "&rest"))
             (setf (gethash (cadr tmpl) ctx) args)
             (when (and (listp (cddr tmpl))
                        (or (stringp (caddr tmpl)) (symbolp (caddr tmpl)))
                        (string= (et-value (caddr tmpl)) "&key"))
               (expand-macro-args args (cddr tmpl) ctx)))
            ((string= (et-value (car tmpl)) "&key")
             (unless (= (mod (length args) 2) 0)
               (format t "warning: odd number of keyword arguments."))
             (extract-keyword-args args (cdr tmpl) ctx))
            (t
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
        (if (or (string= (string (et-value (car tmpl))) "&body")
                (string= (string (et-value (car tmpl))) "&rest"))
            ;; we should clarify where the evaluation of macrofn takes place
            ;; because this may raise issues in the &body case.
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

(defun print-prefix (op form stmtp indent to-stream)
  (when stmtp
    (format to-stream "~v@{~C~:*~}" indent #\Space))
  (cond ((string= op ".++")
         (format to-stream "~a++"
                 (with-output-to-string (s) (print-form form nil 0 s))))
        ((string= op ".--")
         (format to-stream "~a--"
                 (with-output-to-string (s) (print-form form nil 0 s))))
        ((string= op "++.")
         (format to-stream "++~a"
                 (with-output-to-string (s) (print-form form nil 0 s))))
        ((string= op "--.")
         (format to-stream "--~a"
                 (with-output-to-string (s) (print-form form nil 0 s)))))
  (when stmtp
    (format to-stream ";~%")))

(defun print-.++ (form stmtp indent to-stream)
  (print-prefix ".++" (cadr form) stmtp indent to-stream))

(defun print-.-- (form stmtp indent to-stream)
  (print-prefix ".--" (cadr form) stmtp indent to-stream))

(defun print-++. (form stmtp indent to-stream)
  (print-prefix "++." (cadr form) stmtp indent to-stream))

(defun print---. (form stmtp indent to-stream)
  (print-prefix "--." (cadr form) stmtp indent to-stream))

(defvar kwd-behavior nil)

(defun compile-call (form)
  (list* (car form) (mapcar (lambda (x) (compile-form x)) (cdr form))))

(defun simplify-list* (form)
  (when (and form (listp form))
    (destructuring-bind (hd &rest tl) form
      (if tl
          (cons hd (simplify-list* tl))
          (if (and (listp hd)
                   (eq (car hd) +eclisp-quote+))
              (list (cons +eclisp-quote+ (cadr hd)))
              hd)))))

;; todo: fix this awful wart
(defun print-list* (form stmtp indent to-stream)
  (print-form (cons +eclisp-list+ (simplify-list* (cdr form))) stmtp indent to-stream))

(defun print-form (form stmtp indent to-stream)
  "Compile an eclisp FROM and write it on TO-STREAM"
  (if (consp form)
      (if (consp (car form))
          (progn
            (format to-stream "~a (~{~a~^, ~})"
                    (with-output-to-string (s) (print-form (car form) nil 0 s))
                    (mapcar (lambda (x)
                                      (cond ((eq (et-type x) :eclisp-string) (format nil "\"~a\"" (et-value x)))
                                            ((eq (et-type x) :eclisp-character) (format nil "'~a'" (et-value x)))
                                            (t (with-output-to-string (s) (print-form x nil 0 s)))))
                                    (cdr form)))
            (when stmtp (format to-stream ";~%")))
          (destructuring-bind (op &rest args) form
            (declare (ignore args))
            (let ((fn (cdr (assoc op kwd-behavior))))
              (unless (eql fn 'print-prog)
                (setf indent (max 0 indent)))
              (if fn
                  (funcall fn form stmtp indent to-stream)
                  (progn
                    (format to-stream "~v@{~C~:*~}" indent #\Space)
                    (format to-stream "~a (~{~a~^, ~})"
                            (et-value (car form))
                            (mapcar (lambda (x)
                                      (cond ((eq (et-type x) :eclisp-string) (format nil "\"~a\"" (et-value x)))
                                            ((eq (et-type x) :eclisp-character) (format nil "'~a'" (et-value x)))
                                            (t (with-output-to-string (s) (print-form x nil 0 s)))))
                                    (cdr form)))
                    (when stmtp (format to-stream ";~%")))))))
      (progn
        (unless (eql form nil)
          (format to-stream "~v@{~C~:*~}" indent #\Space)
          (cond ((eq (et-type form) :eclisp-string) (format to-stream "\"~a\"" (et-value form)))
                ((eq (et-type form) :eclisp-character) (format to-stream "'~a'" (et-value form)))
                (t (format to-stream "~a" (et-value form))))
              (when stmtp (format to-stream ";~%"))))))

(defun print-eclisp (from-stream to-stream)
  "Write the result of the compilation of the content of FROM-STREAM into
TO-STREAM."
  (eclisp-reader-init)
  (let ((kwd-behavior
         `((,+eclisp-pp-include+       . print-cpp-include)
           (,+eclisp-pp-define+        . print-cpp-define)
           (,+eclisp-pp-if+            . print-cpp-if)
           (,+eclisp-special-type+     . print-as-type)
           (,+eclisp-special-verbatim+ . print-verbatim)
           (,+eclisp-special-comment+  . print-comment)
           (,+eclisp-break+            . print-break)
           (,+eclisp-continue+         . print-continue)
           (,+eclisp-special-funcall+  . print-funcall)
           (,+eclisp-asm+              . print-asm)
           (,+eclisp-cast+             . print-cast)
           (,+eclisp-def+              . print-def)
           (,+eclisp-seq+              . print-seq)
           (,+eclisp-list+             . print-list)
           (,+eclisp-list*+            . print-list*)
           (,+eclisp-prog+             . print-prog)
           (,+eclisp-prog*+            . print-prog*)
           (,+eclisp-dot+              . print-binop)
           (,+eclisp-arrow+            . print-binop)
           (,+eclisp-aref+             . print-aref)
           (,+eclisp-if+               . print-if)
           (,+eclisp-label+            . print-label)
           (,+eclisp-goto+             . print-goto)
           (,+eclisp-ite+              . print-ite)
           (,+eclisp-for+              . print-for)
           (,+eclisp-do-while+         . print-do-while)
           (,+eclisp-while+            . print-while)
           (,+eclisp-switch+           . print-switch)
           (,+eclisp-return+           . print-return)
           (,+eclisp-quote+            . print-quote)
           (,+eclisp-backquote+        . print-backquote)
           (,+eclisp-post-inc+         . print-.++)
           (,+eclisp-pre-inc+          . print-++.)
           (,+eclisp-post-dec+         . print-.--)
           (,+eclisp-pre-dec+          . print---.)
           (,+eclisp-set+              . print-binop)
           (,+eclisp-plus-update+      . print-binop)
           (,+eclisp-minus-update+     . print-binop)
           (,+eclisp-mult-update+      . print-binop)
           (,+eclisp-div-update+       . print-binop)
           (,+eclisp-mod-update+       . print-binop)
           (,+eclisp-and-update+       . print-binop)
           (,+eclisp-xor-update+       . print-binop)
           (,+eclisp-or-update+        . print-binop)
           (,+eclisp-lshift-update+    . print-binop)
           (,+eclisp-rshift-update+    . print-binop)
           (,+eclisp-lt+               . print-cmp-op)
           (,+eclisp-gt+               . print-cmp-op)
           (,+eclisp-lte+              . print-cmp-op)
           (,+eclisp-gte+              . print-cmp-op)
           (,+eclisp-equal+            . print-cmp-op)
           (,+eclisp-neq+              . print-cmp-op)
           (,+eclisp-land+             . print-binop)
           (,+eclisp-lor+              . print-binop)
           (,+eclisp-plus+             . print-binop)
           (,+eclisp-minus+            . print-binop)
           (,+eclisp-mult+             . print-binop)
           (,+eclisp-div+              . print-binop)
           (,+eclisp-mod+              . print-binop)
           (,+eclisp-xor+              . print-binop)
           (,+eclisp-or+               . print-binop)
           (,+eclisp-and+              . print-binop)
           (,+eclisp-neg+              . print-binop)
           (,+eclisp-not+              . print-binop)
           (,+eclisp-lshift+           . print-binop)
           (,+eclisp-rshift+           . print-binop)
           (,+eclisp-macro+            . nil)
           (,+eclisp-macrofn+          . nil)
           (,+eclisp-macrolet+         . nil))))
    (loop for form = (eclisp-read from-stream)
          while form do
            (print-form (eclisp-eval form nil) t -1 to-stream)))
  (setf +eclisp-readtable+ (make-eclisp-readtable :content '() :dtable '())))

(defun main ()
  "The entry point."
  (let ((args sb-ext:*posix-argv*))
    (if (cdr args)
        (with-open-file (f (cadr args))
          (print-eclisp f *standard-output*))
        (print-eclisp *standard-input* *standard-output*))))

;; below: rework

(defmacro eclisp-indent (ostream n)
  `(format ,ostream "~v@{~C~:*~}" ,n #\Space))

(defun mapdive (f list)
  (when list
    (cond ((atom list) (funcall f list))
          (t (if (atom (car list))
                 (cons (funcall f (car list)) (mapdive f (cdr list)))
                 (cons (mapdive f (car list)) (mapdive f (cdr list))))))))

(defvar +c-keywords+
  '("auto" "break" "case" "char" "const" "continue" "default" "do" "double"
    "else" "enum" "extern" "float" "for" "goto" "if" "inline" "int" "long"
    "register" "return" "short" "signed" "sizeof" "static" "struct" "switch"
    "typedef" "union" "unsigned" "void" "volatile" "while"))

(defun print-c-type (name type acc)
  "Create a nested list which represents the variable NAME of TYPE.
ACC should be NIL at first."
  (labels
      ((print-args-aux (tt)
         (list ", "
               (cond
                 ((atom tt) (print-c-type nil tt nil))
                 ((and (atom (car tt))
                       (not (member (et-value (car tt)) +c-keywords+
                                    :test #'string=)))
                  (let ((rev-tt (reverse tt)) names typ)
                    (setf typ (car rev-tt))
                    (setf names (reverse (cdr rev-tt)))
                    ((lambda (l) (cons (cdar l) (cdr l)))
                     (loop
                       for n in names
                       collect (list ", " (print-c-type (et-value n) typ nil))))))
                 ((and (atom (car tt)) (member (et-value (car tt)) +c-keywords+
                                                  :test #'string=))
                  (print-c-type nil tt nil))
                 (t (print-c-type nil (car tt) nil)))))
       (print-args (args)
         (list "("
               ((lambda (l) (cons (cdar l) (cdr l)))
                (loop for tt in args collect (print-args-aux tt)))
               ")"))
       (handle-struct-field (tt)
         (cond ((eq (type-of (car tt)) 'eclisp-token)
                (let* ((rev-tt (reverse tt))
                       (doc (if (eq (et-type (car rev-tt)) :eclisp-string) (car rev-tt))))
                  (append
                   (when doc
                     (list (with-output-to-string (s)
                             (format s "/* ~a */" (et-value doc)))
                           #\Newline))
                   (if (member (et-value (car tt)) +c-keywords+ :test #'string=)
                       (let ((ntt (if doc (reverse (cdr rev-tt)))))
                         (print-c-type nil ntt nil))
                       (let ((typ (if doc (cadr rev-tt) (car rev-tt)))
                             (ntt (reverse (if doc (cddr rev-tt) (cdr rev-tt)))))
                         (handle-struct-compound-field ntt typ))))))
               (t (print-c-type nil (car tt) nil))))
       (handle-struct-compound-field (ntt typ)
         ((lambda (l) (cons (cddar l) (cdr l)))
          (loop for var in (group-bitfield ntt)
                collect
                (list
                 ";" #\Newline
                 (print-c-type
                  (if (cadr var)
                      (format nil "~a : ~a" (et-value (car var))
                              (with-output-to-string (s)
                                (print-form (cadr var) nil 0 s)))
                      (et-value (car var)))
                  typ nil))))))
    (let ((ptr-kwd (intern-eclisp-token "ptr"))
          (fun-kwd (intern-eclisp-token "->"))
          (arr-kwd (intern-eclisp-token "array"))
          (typeof-kwd (intern-eclisp-token "type-of"))
          (struct-kwd (intern-eclisp-token "struct"))
          (union-kwd (intern-eclisp-token "union"))
          (enum-kwd (intern-eclisp-token "enum")))
      (if (consp type)
          (let ((kind (car type)))
            (cond
              ((eq ptr-kwd kind)
               (print-c-type
                nil
                (if (consp (cadr type)) (cadr type) (cdr type))
                (list "(*" acc name ")")))
              ((and (eq arr-kwd kind) (cdr type))
               (print-c-type
                nil
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
              ((and (eq kind typeof-kwd) (cdr type))
               (print-c-type
                 name
                 nil
                 (list "__typeof (" (with-output-to-string (s) (print-form (cadr type) nil 0 s)) ")")))
              ((eq fun-kwd kind)
               (print-c-type
                nil
                (cadr type)
                (list "(" acc name ")" (print-args (cddr type)))))
              ((eq enum-kwd kind)
               (print-c-type
                name kind
                (let* ((enum-anon-p (consp (cadr type)))
                       (enum-name (if enum-anon-p nil (list (et-value (cadr type)))))
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
                                                      (list (et-value (car tt)) " = "
                                                            (cond
                                                              ((eq (et-type (cadr tt)) :eclisp-string)
                                                                     (format nil "\"~a\"" (et-value (cadr tt))))
                                                              ((eq (et-type (cadr tt)) :eclisp-character)
                                                                     (format nil "'~a'" (et-value (cadr tt))))
                                                              (t (format nil "~a" (et-value (cadr tt))))))

                                                      (list (et-value tt))))))
                             (list #\Newline "}")))
                          acc))))
              ((member kind `(,struct-kwd ,union-kwd))
               (print-c-type
                name kind
                (let* ((struct-anon-p (consp (cadr type)))
                       (struct-name (if struct-anon-p nil (list (et-value (cadr type)))))
                       (struct-contents (if struct-anon-p (cdr type) (cddr type))))
                  (append struct-name
                          (when struct-contents
                            (append
                             (when struct-name (list #\Newline))
                             (list "{")
                             ((lambda (l) (append (cons (cdar l) (cdr l)) (list ";")))
                              (loop for tt in struct-contents
                                    collect (list ";" #\Newline (handle-struct-field tt))))
                             (list #\Newline "}")))
                          acc))))
              ((member (et-value kind) '("volatile" "const") :test #'string=)
               (print-c-type
                nil
                (if (consp (cadr type)) (cadr type) (cdr type))
                (list (et-value kind) " " acc name)))
              ((member (et-value kind) '("typedef" "static" "inline" "register" "extern"
                                         "long" "short" "signed" "unsigned")
                       :test #'string=)
               (append (list (et-value kind) " ")
                       (print-c-type name (if (consp (cadr type)) (cadr type) (cdr type)) acc)))
              (t (append (list (et-value kind))
                         (if (null acc) nil (list " "))
                         acc
                         (if (null name) nil (list " "))
                         (list name)))))
          (append (list (et-value type))
                  (if (null acc) nil (list " ")) acc (if (null name) nil (list " ")) (list name))))))

(defun neo-print-type (name type to-stream)
  "Compile TYPE and write it on TO-STREAM."
  (print-ll (print-c-type name type nil) 0 to-stream))

(defgeneric eclisp-print (ostream op args indent stmtp))

(defmethod no-applicable-method (eclisp-print &rest args)
  (if (eql eclisp-print #'eclisp-print)
  (destructuring-bind (ostream op op-args indent stmtp) args
      (eclisp-print ostream 'eclisp-call (cons op op-args) indent stmtp))))

(defmethod eclisp-print (ostream (op (eql +eclisp-pp-include+)) args indent stmtp)
  (loop for arg in args
        do (eclisp-indent ostream indent)
           (if (eq (et-type arg) :eclisp-string)
               (format ostream "#include ~s~%" (et-value arg))
               (format ostream "#include ~a~%" (et-value arg)))))

(defmethod eclisp-print (ostream (op (eql +eclisp-pp-define+)) args indent stmtp)
  (eclisp-indent ostream indent)
  (if (cadr args)
      (format ostream "#define ~a ~a~%"
              (et-value (car args)) (et-value (cadr args)))
      (format ostream "#define ~a~%" (et-value (car args)))))

(defmethod eclisp-print (ostream (op (eql +eclisp-def+)) form indent stmtp)
  (destructuring-bind (var type value documentation) form
    (unless (null documentation)
      (progn
        (eclisp-indent ostream indent)
        (format ostream "/* ~a  */~%"
                (regex-replace-all "\\n" (et-value documentation)
                                   (concatenate 'string '(#\Newline) "   "
                                                (eclisp-indent nil indent))))))
    (when var
      (when (string= (gethash "linkage" (es-attrs var)) "static")
        (setf type `(,(intern-eclisp-token "static") ,type))))
    (eclisp-indent ostream indent)
    (neo-print-type (when var (es-name var)) type ostream)
    (when value (format ostream " = ~a" (et-value value)))
    ;; (when value (format to-stream " = ~a"
    ;;                     (with-output-to-string (s) (print-form value nil 0 s))))
    (format ostream ";~%")))

(defmethod eclisp-print (ostream (op (eql +eclisp-dot+)) form indent stmtp)
  (if stmtp
      (eclisp-indent ostream indent))
  (let ((cur form))
    (loop while cur do
      (if (atom (car cur))
          (eclisp-print ostream 'atom (car cur) indent nil)
          (eclisp-print ostream (car cur) (cadr cur) indent nil))
          (if (cdr cur) (format ostream "."))
          (setf cur (cdr cur))))
  (if stmtp
      (format ostream ";~%")))

(defmethod eclisp-print (ostream (op (eql +eclisp-arrow+)) form indent stmtp)
  (if stmtp
      (eclisp-indent ostream indent))
  (let ((cur form))
    (loop while cur do
      (if (atom (car cur))
          (eclisp-print ostream 'atom (car cur) indent nil)
          (eclisp-print ostream (car cur) (cadr cur) indent nil))
          (if (cdr cur) (format ostream "->"))
          (setf cur (cdr cur))))
  (if stmtp
      (format ostream ";~%")))

(defmethod eclisp-print (ostream (op (eql 'atom)) form indent stmtp)
  (if stmtp
      (eclisp-indent ostream indent))
  (format ostream "~a" (et-value form))
  (if stmtp
      (format ostream ";~%")))

(defun test-eclisp (s ostream)
  "Write the result of the compilation of the content of FROM-STREAM into
TO-STREAM."
  (with-input-from-string (is s)
    (loop for form = (eclisp-read is)
          while form
          do (if (listp form)
                 (eclisp-print ostream (car form) (cdr form) 0 t)
                 (eclisp-print ostream 'atom form 0 t)))))

(defun neo-main ()
  "The main entry point of the refactored code."
  (eclisp-reader-init)
  (print-eclisp *standard-input* *standard-output*))
