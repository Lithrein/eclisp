;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

(in-package :eclisp)

(deftype eclisp-token-type ()
  '(member :eclisp-string :eclisp-number :eclisp-symbol :eclisp-character))

(defclass eclisp-token ()
  ((value             :initform (error "value: mandatory argument.")
                      :initarg :value
                      :accessor et-value
                      :documentation
                      "The VALUE of the token.  Strings are stored without surrounding braces.")
   (type                :initform (error "type: mandatory argument.")
                        :initarg :type
                        :accessor et-type
                        :type eclisp-token-type
                        :documentation
                        "The TYPE of token: string, number, symbol")
   (properties          :initform (make-hash-table :test #'equal)
                        :initarg :properties
                        :accessor et-properties
                        :documentation "A collection of properties about a symbol.
Only :eclisp-symbol can have properties.")
   (parse-fn            :initform nil
                        :initarg :parse-fn
                        :accessor et-parse
                        :documentation
                        "Tokens associated with eClisp special forms have a PARSE-FN.
It is used to process the arguments of the special form and produces
an internal eClisp object."))
  (:documentation
   "The eClisp reader produces ECLISP-TOKENs.  They are proccessed by the eClisp
parser into high-order data-structures, and are also the data-type which
eClisp macros work on."))

(defmethod print-object ((obj eclisp-token) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((value et-value)
                     (type et-type)
                     (props et-properties))
        obj
      (format stream "~A#~A attrs: {~{~{(~a: ~a)~}~^~}}" value
              (ecase type
                (:eclisp-string    "str")
                (:eclisp-number    "num")
                (:eclisp-symbol    "sym")
                (:eclisp-character "car"))
              (loop for key being the hash-keys of props
                    using (hash-value value)
                    collect (list key value))))))

(defstruct scope
  typ
  (syms (make-hash-table))
  (structs (make-hash-table))
  (unions (make-hash-table))
  (enums (make-hash-table)))

(defvar *eclisp-tokens* (make-hash-table :test #'equal)
  "Hash table which stores all previously allocated tokens.
This allows to reuse previous instances with the same VALUE and TYPE, and allows
eClisp token to be compared with #'eq.")

(defun intern-eclisp-token (value &optional (type :eclisp-symbol) (fn nil))
  "Register a new eClisp token or return an already allocated eClisp
token with the same content.  This allows tokens to be compared with #'eq."
  (declare (type (or string number character) value)
           (type eclisp-token-type type))
  (let* ((key (format nil "~s:~s" value type))
         (tok (gethash key *eclisp-tokens*)))
    (unless tok
      (setf tok (make-instance 'eclisp-token :value value
                               :type type
                               :parse-fn fn))
      (setf (gethash key *eclisp-tokens*) tok))
    (with-accessors ((parse-fn et-parse)) tok
      (unless (and (null fn) parse-fn)
        (setf parse-fn fn)))
    tok))

;; . ->
;; = += -= *= /= %= &= ^= |= <<= >>=
;; + - * / % ^ | & << >> && ||
;; < > <= >= == != (cmpop)
(defclass eclisp-binary-op ()
  ((op)
   (op-str)
   (left-spacing)
   (right-spacing)
   (parenthezise)))

(defclass eclisp-unary-op ()
  ((op)
   (op-str)
   (spacing)
   (parenthesize)
   (position)))

(declaim (inline parse-call))
(defun parse-call (form ctx)
  (mapcar (lambda (x) (eclisp-eval x ctx)) form))

(defun eclisp-macroexpand-1 (form ctx)
  (declare (ignore ctx))
   (if (consp form)
       (destructuring-bind (op &rest args) form
         (if (gethash op macro-tbl)
             (setf form (expand-macro op args (make-hash-table) ctx))
             form))
       (if (gethash form symbol-macro-tbl)
           (setf form (expand-macro form nil))
           form)))

(defun eclisp-macroexpand (form ctx)
  (loop
   (if (consp form)
       (destructuring-bind (op &rest args) form
         (if (gethash op macro-tbl)
             (setf form (expand-macro op args (make-hash-table) ctx))
             (return form)))
       (if (gethash form symbol-macro-tbl)
           (setf form (expand-macro form nil))
           (return form)))))

(defun eclisp-eval (form ctx)
  (let ((xform (eclisp-macroexpand form ctx)))
    (cond ((atom xform) xform)
          (t (destructuring-bind (op &rest args) xform
               (declare (ignore args))
               (if (et-parse op)
                   (funcall (et-parse op) xform ctx)
                   (parse-call xform ctx)))))))

;; Preprocessor symbols
;; Preprocessor symbols are prefixed by the digraph for sharp (%:).

(flet ((eclisp-pp-include-parse (form ctx)
         (let ((eclisp-include (pop form)))
           (cons eclisp-include
                 (remove nil
                         (mapcar (lambda (x)
                                   (cond ((atom x)
                                          (let ((file (eclisp-eval x ctx)))
                                            (cond ((eclisp-file-p (et-value file))
                                                   (read-macro-definitions (et-value file))
                                                   (intern-eclisp-token (convert-file-extension (et-value file))
                                                                        (et-type file)))
                                                  (t file))))
                                         (t
                                          (cond
                                            ((and (consp x) (= 2 (length x)))
                                             (let ((orig (eclisp-eval (car x) ctx))
                                                   (dest (eclisp-eval (cadr x) ctx)))
                                               (if (eclisp-file-p (et-value orig))
                                                   (read-macro-definitions (et-value orig)))
                                               (intern-eclisp-token (et-value dest) (et-type orig))))
                                            ((and (consp x) (= 1 (length x)))
                                             (let ((orig (eclisp-eval (car x) ctx)))
                                               (if (eclisp-file-p (et-value orig))
                                                   (read-macro-definitions (et-value orig)))
                                               nil))
                                            (t (raise "Expected a list with 1 or 2 elements"))))))
                                 form))))))
  (define-constant +eclisp-pp-include+
      (intern-eclisp-token "%:include"
                           :eclisp-symbol
                           #'eclisp-pp-include-parse)
    "Keyword associated with the preprocessor C/eClisp include facility.
This form allows a list of C header files to be included (copy-paste
semantics) in the current eClisp file.  If the included file is an eClisp file,
macros from this file are loaded in the current environment."))

(flet ((eclisp-pp-define-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-pp-define+
      (intern-eclisp-token "%:define"
                           :eclisp-symbol
                           #'eclisp-pp-define-parse)
    "Keyword associated with the preprocessor define facility.
This form allows to define preprocessor constant for the underlying C processor.
Only trivial constants can be defined through this mean.  More sophisticated C
macro should be done with the verbatim facility or should be defined as eClisp
macros."))

(flet ((eclisp-pp-if-parse (form ctx &aux res)
         (let ((pp-if (pop form)))
           (do ((cur form (cdr cur)) (first t nil))
               ((not cur))
             (destructuring-bind (cond &rest body) (car cur)
               (push (list* (eclisp-eval cond ctx)
                            (loop for b in body
                                  collect (eclisp-eval b ctx)))
                     res)))
           (cons pp-if (reverse res)))))
  (define-constant +eclisp-pp-if+
      (intern-eclisp-token "%:if"
                           :eclisp-symbol
                           #'eclisp-pp-if-parse)
    "Keyword associated with the preprocessor conditionals.
This form is used like the COND form, hence we do not provide #else, #elsif
and #endif.  #ifdef and #ifndef should be emulated by using the pseudo function
`defined'."))

;; Special symbols
;; Special symbols are prefixed by a percent (%) sign.

(flet ((eclisp-special-type-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-special-type+
      (intern-eclisp-token "%type"
                           :eclisp-symbol
                           #'eclisp-special-type-parse)
    "This special form explicitely marks its argument as an eClisp type."))

(flet ((eclisp-special-verbatim-parse (form ctx)
         (declare (ignore ctx))
         form))
  (define-constant +eclisp-special-verbatim+
      (intern-eclisp-token "%verbatim"
                           :eclisp-symbol
                           #'eclisp-special-verbatim-parse)
    "This special form allows the embed of arbitrary C code."))

(flet ((eclisp-special-comment-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-special-comment+
      (intern-eclisp-token "%comment"
                           :eclisp-symbol
                           #'eclisp-special-comment-parse)
    "This special form declare a comment that should be propagated into
the final C source code."))

(flet ((eclisp-special-funcall-parse (form ctx)
         (parse-call (cdr form) ctx)))
  (define-constant +eclisp-special-funcall+
      (intern-eclisp-token "%funcall"
                           :eclisp-symbol
                           #'eclisp-special-funcall-parse)
    "This special form takes the name of a function as a symbol and a list
of arguments.  This is useful for breaking the recursion in some macros."))

;; Normal eClisp keywords with a direct C equivalent

(flet ((eclisp-break-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-break+
      (intern-eclisp-token "break"
                           :eclisp-symbol
                           #'eclisp-break-parse)))

(flet ((eclisp-continue-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-continue+
      (intern-eclisp-token "continue"
                           :eclisp-symbol
                           #'eclisp-continue-parse)))

(flet ((eclisp-cast-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-cast+
      (intern-eclisp-token "cast"
                           :eclisp-symbol
                           #'eclisp-cast-parse)))

(defun neo-compile-symbol-definition (symdef)
  "Reads an (optional) name and its attributes.
All this information is returned as an ECLISP-SYMBOL instance"
  (if (listp symdef)
      (let* ((sym (make-instance 'eclisp-symbol :name nil))
             (cur-key nil))
        (unless (and symdef (char= (aref (et-value (car symdef)) 0) #\:))
          (setf (es-name sym) (et-value (car symdef)))
          (pop symdef))
        (loop for elt in symdef do
              (cond ((and (stringp (et-value elt)) (char= (aref (et-value elt) 0) #\:))
                     (setf cur-key (subseq (et-value elt) 1))
                     (setf (gethash cur-key (es-attrs sym)) nil))
                    (t
                     (if (null cur-key)
                         (error "cur-key is nil")
                         (setf (gethash cur-key (es-attrs sym)) (et-value elt))))))
        sym)
      (make-instance 'eclisp-symbol :name (et-value symdef))))

(defun compute-full-type-field (field ctx &optional registerp)
  ;; (format t "cftf(in): ~a~%" field)
  (let* ((rfield (reverse field))
         (docp   (eq (et-type (car rfield)) :eclisp-string))
         (typ    (if docp (cadr rfield) (car rfield)))
         (fields (if docp (cddr rfield) (cdr rfield))))
    (when registerp
      (register-type typ ctx))
    (loop for f in fields collect (list f (compute-full-type typ ctx)))))

(defun lookup-type (type ctx)
  (when ctx
    (or (cond
          ((string= (et-value (car type)) "struct")
           (gethash (cadr type) (scope-structs (car ctx))))
          ((string= (et-value (car type)) "union")
           (gethash (cadr type) (scope-unions (car ctx))))
          ((string= (et-value (car type)) "enum")
           (gethash (cadr type) (scope-enum (car ctx)))))
        (lookup-type type (cdr ctx)))))

(defvar *full-type-hcons* (make-hash-table :test #'equal))

(defun compute-full-type (type ctx &optional registerp)
  (if (atom type)
      type
      (let ((len (length type)))
        (cond
          ((or (string= (et-value (car type)) "struct")
               (string= (et-value (car type)) "union"))
           (cond
             ((and (atom (cadr type))
                   (not (atom (caddr type)))
                   (not (null (cddr type))))
              ;; (struct/union atom/name ...)
              (if (gethash (cadr type) *full-type-hcons*)
                  (if registerp
                      (nconc (gethash (cadr type) *full-type-hcons*)
                             (loop for field in (cddr type)
                                   nconc (compute-full-type-field field ctx registerp)))
                      (gethash (cadr type) *full-type-hcons*))
                  (progn
                    (setf (gethash (cadr type) *full-type-hcons*) (list (car type) (cadr type)))
                    (nconc (gethash (cadr type) *full-type-hcons*)
                           (loop for field in (cddr type)
                                 nconc (compute-full-type-field field ctx registerp))))))
             ((and (listp (cadr type))
                   (not (atom (caddr type)))
                   (not (null (cdr type))))
              ;; (struct/union ...)
              (list* (car type)
                     (loop for field in (cdr type)
                           nconc (compute-full-type-field field ctx registerp))))
             (t (let ((ft (lookup-type type ctx)))
                  (if ft
                       (let* ((ntype (gethash (cadr type) *full-type-hcons*))
                              (struct-or-union (car ntype))
                              (type-name (make-instance 'eclisp-token :value (et-value (cadr type)) :type (et-type (cadr type))))
                              (type-content (cddr ntype)))
                       (do ((attrs-list (cddr type) (cddr attrs-list)))
                           ((not (and (not (null (car attrs-list)))
                                      (atom (car attrs-list))
                                      (char= #\: (aref (et-value (car attrs-list)) 0)))))
                         (setf (gethash (car attrs-list) (et-properties type-name)) (cadr attrs-list)))
                       (list* struct-or-union type-name type-content))
                      (if (gethash (cadr type) *full-type-hcons*)
                          (gethash (cadr type) *full-type-hcons*)
						  (setf (gethash (cadr type) *full-type-hcons*)
						    (list (car type) (cadr type)))))))))
          ((string= (et-value (car type)) "array")
           (cond
             ((= len 3) (list (car type) (cadr type) (compute-full-type (caddr type) ctx registerp)))
             ((= len 2) (list (car type) (compute-full-type (cadr type) ctx registerp)))
             (t (error "array expects 1 or 2 arguments."))))
          ((string= (et-value (car type)) "ptr")
           (list (car type) (if (listp (cadr type))
                                (compute-full-type (cadr type) ctx registerp)
                                (compute-full-type (cdr type) ctx registerp))))
          (t (multiple-value-bind (val present-p) (gethash (car type) (scope-syms (car ctx)))
               (if present-p
                   (compute-full-type val ctx registerp)
                   type)))))))

(defun register-type (type ctx)
  (when (and (listp type)
             (or (string= (et-value (car type)) "struct")
                 (string= (et-value (car type)) "union")
                 (string= (et-value (car type)) "enum"))
             (atom (cadr type))
             (not (null (cddr type))))
   (cond
    ((string= (et-value (car type)) "struct")
     (setf (gethash (cadr type) (scope-structs (car ctx))) (compute-full-type type ctx t)))
    ((string= (et-value (car type)) "union")
     (setf (gethash (cadr type) (scope-unions (car ctx))) (compute-full-type type ctx t)))
    ((string= (et-value (car type)) "enum")
     (setf (gethash (cadr type) (scope-enums (car ctx))) (compute-full-type type ctx t))))))

(labels
    ((parse-defvar (var type rest ctx &aux value documentation)
       (if var
           (setf value (car rest)
                 documentation (cadr rest))
           (setf documentation (car rest)))
       (when (and (stringp (et-value (car type)))
                  (string= (et-value (car type)) "typedef"))
         (setf value nil)
         (when (eq (et-type (car rest)) :eclisp-string)
           (setf documentation (car rest))))
       (cond
         ((string= (et-value (car type)) "typedef")
          (register-type (cdr type) ctx))
         (t (register-type type ctx)))
       (when var
         (let ((binding (make-instance 'eclisp-token
                                       :value (es-value var)
                                       :type :eclisp-symbol
                                       :parse-fn nil
                                       :properties (make-hash-table :test #'equal))))
           (setf (gethash "type" (et-properties binding)) (compute-full-type type ctx))
           ;;(format t "ft(~a): ~a~%" type (gethash "type" (et-properties binding)))
           (setf (gethash (es-name var) (scope-syms (car ctx))) binding)))
       (when value
         (setf value (eclisp-eval value (cons (make-scope :typ `("def" . ,(es-name var))) ctx))))
       (list var type value documentation))
     (parse-defun (var type rest ctx &aux body documentation)
       (if (and rest (listp rest)
                (atom (car rest))
                (eq (et-type (car rest)) :eclisp-string))
           (setf documentation (car rest)
                 body (cdr rest))
           (setf body rest))
       (let ((properties (make-instance 'eclisp-token
                                        :value (es-name var)
                                        :type :eclisp-symbol
                                        :parse-fn nil
                                        :properties (make-hash-table :test #'equal))))
         (setf (gethash "type" (et-properties properties)) type)
         (setf (gethash (es-name var) (scope-syms (car ctx))) properties)
         (when body
           (push (make-scope :typ `("function" . ,(es-name var))) ctx)
           (loop for item in type do
                 (when (listp item)
                   (let* ((typelist (reverse item))
                          (param-type (car typelist))
                          (params (cdr typelist)))
                     (loop for param-name in params do
                           (let ((param (make-instance 'eclisp-token
                                                       :value (et-value param-name)
                                                       :type :eclisp-symbol
                                                       :parse-fn nil
                                                       :properties (make-hash-table :test #'equal))))
                             (setf (gethash "type" (et-properties param))
                                   (compute-full-type param-type ctx))
                             (setf (gethash (et-value param-name) (scope-syms (car ctx))) param))))))))
       (setf body (mapcar (lambda (f) (eclisp-eval f ctx)) body))
       (pop ctx)
       (list var type documentation body))
     (eclisp-def-parse (form ctx &aux var type rest)
       (let ((eclisp-def (pop form)))
         (cond ((and (consp (car form))
                     (member (et-value (caar form)) +c-keywords+ :test #'equal))
                (setf type (car form)
                      rest (cdr form)))
               (t
                (setf var (neo-compile-symbol-definition (car form)))
                ;; todo: fix (list (cadr form))
                (setf type (if (consp (cadr form)) (cadr form) (list (cadr form))))
                (setf (es-type var) type)
                (setf rest (cddr form))))
         (let ((expanded-type (if type (eclisp-eval type ctx))))
           (if (atom expanded-type) (setf expanded-type (list expanded-type)))
           (cons eclisp-def
                 (cond ((and expanded-type (eq (car expanded-type) +eclisp-arrow+))
                        (parse-defun var expanded-type rest ctx))
                       (t (parse-defvar var expanded-type rest ctx))))))))
  (define-constant +eclisp-def+
      (intern-eclisp-token "def"
                           :eclisp-symbol
                           #'eclisp-def-parse)))

(flet ((eclisp-dot-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-dot+
      (intern-eclisp-token "."
                           :eclisp-symbol
                           #'eclisp-dot-parse)))

(flet ((eclisp-arrow-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-arrow+
      (intern-eclisp-token "->"
                           :eclisp-symbol
                           #'eclisp-arrow-parse)))

(flet ((eclisp-aref-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-aref+
      (intern-eclisp-token "aref"
                           :eclisp-symbol
                           #'eclisp-aref-parse)))

(flet ((eclisp-if-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-if+
      (intern-eclisp-token "if"
                           :eclisp-symbol
                           #'eclisp-if-parse)))

(flet ((eclisp-label-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-label+
      (intern-eclisp-token "label"
                           :eclisp-symbol
                           #'eclisp-label-parse)))

(flet ((eclisp-goto-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-goto+
      (intern-eclisp-token "goto"
                           :eclisp-symbol
                           #'eclisp-goto-parse)))

(flet ((eclisp-ite-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-ite+
      (intern-eclisp-token "?:"
                           :eclisp-symbol
                           #'eclisp-ite-parse)))

(flet ((eclisp-for-parse (form ctx)
         (parse-call form (cons (make-scope :typ '("block" . "for")) ctx))))
  (define-constant +eclisp-for+
      (intern-eclisp-token "for"
                           :eclisp-symbol
                           #'eclisp-for-parse)))

(flet ((eclisp-do-while-parse (form ctx)
         (parse-call form (cons (make-scope :typ '("block" . "do-while")) ctx))))
  (define-constant +eclisp-do-while+
      (intern-eclisp-token "do-while"
                           :eclisp-symbol
                           #'eclisp-do-while-parse)))

(flet ((eclisp-while-parse (form ctx)
         (parse-call form (cons (make-scope :typ '("block" . "while")) ctx))))
  (define-constant +eclisp-while+
      (intern-eclisp-token "while"
                           :eclisp-symbol
                           #'eclisp-while-parse)))

(flet ((eclisp-asm-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-asm+
      (intern-eclisp-token "asm"
                           :eclisp-symbol
                           #'eclisp-asm-parse)))

(labels
    ((parse-label (label ctx)
       (if (eq label (intern-eclisp-token "default"))
           (intern-eclisp-token "default")
           (eclisp-eval label ctx)))
     (parse-label-clauses (label-clauses ctx)
       (destructuring-bind (label &rest clauses) label-clauses
         (list* (if (consp label)
                    (loop for l in label collect (parse-label l ctx))
                    (parse-label label ctx))
                (mapcar (lambda (form) (eclisp-eval form ctx)) clauses))))
     (eclisp-switch-parse (form ctx)
       (list* (car form)
              (eclisp-eval (cadr form) ctx)
              (loop for label-clauses in (cddr form)
                    collect (parse-label-clauses label-clauses ctx)))))
  (define-constant +eclisp-switch+
      (intern-eclisp-token "switch"
                           :eclisp-symbol
                           #'eclisp-switch-parse)))

(flet ((eclisp-return-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-return+
      (intern-eclisp-token "return"
                           :eclisp-symbol
                           #'eclisp-return-parse)))

;; Normal eClisp constructs without a direct C equivalent

(flet ((eclisp-seq-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-seq+
      (intern-eclisp-token "seq"
                           :eclisp-symbol
                           #'eclisp-seq-parse)))

(flet ((eclisp-prog-parse (form ctx)
         (parse-call form (cons (make-scope :typ '("block" . "prog")) ctx))))
  (define-constant +eclisp-prog+
      (intern-eclisp-token "prog"
                           :eclisp-symbol
                           #'eclisp-prog-parse)))

(flet ((eclisp-prog*-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-prog*+
      (intern-eclisp-token "prog*"
                           :eclisp-symbol
                           #'eclisp-prog*-parse)))

;; eClisp operators

(flet ((eclisp-post-inc-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-post-inc+
      (intern-eclisp-token ".++"
                           :eclisp-symbol
                           #'eclisp-post-inc-parse)))

(flet ((eclisp-pre-inc-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-pre-inc+
      (intern-eclisp-token "++."
                           :eclisp-symbol
                           #'eclisp-pre-inc-parse)))

(flet ((eclisp-post-dec-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-post-dec+
      (intern-eclisp-token ".--"
                           :eclisp-symbol
                           #'eclisp-post-dec-parse)))

(flet ((eclisp-pre-dec-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-pre-dec+
      (intern-eclisp-token "--."
                           :eclisp-symbol
                           #'eclisp-pre-dec-parse)))


(flet ((eclisp-set-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-set+
      (intern-eclisp-token "="
                           :eclisp-symbol
                           #'eclisp-set-parse)))

(flet ((eclisp-plus-update-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-plus-update+
      (intern-eclisp-token "+="
                           :eclisp-symbol
                           #'eclisp-plus-update-parse)))

(flet ((eclisp-minus-update-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-minus-update+
      (intern-eclisp-token "-="
                           :eclisp-symbol
                           #'eclisp-minus-update-parse)))

(flet ((eclisp-mult-update-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-mult-update+
      (intern-eclisp-token "*="
                           :eclisp-symbol
                           #'eclisp-mult-update-parse)))

(flet ((eclisp-div-update-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-div-update+
      (intern-eclisp-token "/="
                           :eclisp-symbol
                           #'eclisp-div-update-parse)))

(flet ((eclisp-mod-update-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-mod-update+
      (intern-eclisp-token "%="
                           :eclisp-symbol
                           #'eclisp-mod-update-parse)))

(flet ((eclisp-and-update-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-and-update+
      (intern-eclisp-token "&="
                           :eclisp-symbol
                           #'eclisp-and-update-parse)))

(flet ((eclisp-xor-update-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-xor-update+
      (intern-eclisp-token "^="
                           :eclisp-symbol
                           #'eclisp-xor-update-parse)))

(flet ((eclisp-or-update-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-or-update+
      (intern-eclisp-token "|="
                           :eclisp-symbol
                           #'eclisp-or-update-parse)))

(flet ((eclisp-lshift-update-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-lshift-update+
      (intern-eclisp-token "<<="
                           :eclisp-symbol
                           #'eclisp-lshift-update-parse)))

(flet ((eclisp-rshift-update-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-rshift-update+
      (intern-eclisp-token ">>="
                           :eclisp-symbol
                           #'eclisp-rshift-update-parse)))

(flet ((eclisp-lt-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-lt+
      (intern-eclisp-token "<"
                           :eclisp-symbol
                           #'eclisp-lt-parse)))

(flet ((eclisp-gt-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-gt+
      (intern-eclisp-token ">"
                           :eclisp-symbol
                           #'eclisp-gt-parse)))

(flet ((eclisp-lte-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-lte+
      (intern-eclisp-token "<="
                           :eclisp-symbol
                           #'eclisp-lte-parse)))

(flet ((eclisp-gte-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-gte+
      (intern-eclisp-token ">="
                           :eclisp-symbol
                           #'eclisp-gte-parse)))

(flet ((eclisp-equal-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-equal+
      (intern-eclisp-token "=="
                           :eclisp-symbol
                           #'eclisp-equal-parse)))

(flet ((eclisp-neq-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-neq+
      (intern-eclisp-token "!="
                           :eclisp-symbol
                           #'eclisp-neq-parse)))

(flet ((eclisp-land-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-land+
      (intern-eclisp-token "&&"
                           :eclisp-symbol
                           #'eclisp-land-parse)))

(flet ((eclisp-lor-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-lor+
      (intern-eclisp-token "||"
                           :eclisp-symbol
                           #'eclisp-lor-parse)))

(flet ((eclisp-plus-parse (form ctx)
                                        ; (intern-eclisp-token (apply #'+ (mapcar #'(lambda (arg) (et-value (eclisp-eval arg ctx))) (cdr form))) :eclisp-number)
         (parse-call form ctx)))
  (define-constant +eclisp-plus+
      (intern-eclisp-token "+"
                           :eclisp-symbol
                           #'eclisp-plus-parse)))

(flet ((eclisp-minus-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-minus+
      (intern-eclisp-token "-"
                           :eclisp-symbol
                           #'eclisp-minus-parse)))

(flet ((eclisp-mult-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-mult+
      (intern-eclisp-token "*"
                           :eclisp-symbol
                           #'eclisp-mult-parse)))

(flet ((eclisp-div-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-div+
      (intern-eclisp-token "/"
                           :eclisp-symbol
                           #'eclisp-div-parse)))

(flet ((eclisp-mod-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-mod+
      (intern-eclisp-token "%"
                           :eclisp-symbol
                           #'eclisp-mod-parse)))

(flet ((eclisp-xor-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-xor+
      (intern-eclisp-token "^"
                           :eclisp-symbol
                           #'eclisp-xor-parse)))

(flet ((eclisp-or-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-or+
      (intern-eclisp-token "|"
                           :eclisp-symbol
                           #'eclisp-or-parse)))

(flet ((eclisp-and-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-and+
      (intern-eclisp-token "&"
                           :eclisp-symbol
                           #'eclisp-and-parse)))

(flet ((eclisp-neg-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-neg+
      (intern-eclisp-token "~"
                           :eclisp-symbol
                           #'eclisp-neg-parse)))

(flet ((eclisp-not-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-not+
      (intern-eclisp-token "!"
                           :eclisp-symbol
                           #'eclisp-not-parse)))

(flet ((eclisp-lshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-lshift+
      (intern-eclisp-token "<<"
                           :eclisp-symbol
                           #'eclisp-lshift-parse)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-rshift+
      (intern-eclisp-token ">>"
                           :eclisp-symbol
                           #'eclisp-rshift-parse)))

(flet ((eclisp-macro-parse (form ctx
                                 &aux documentation body)
         (declare (ignore ctx))
         (destructuring-bind (m name args &rest rest) form
           (declare (ignore m))
           (cond ((and (atom (car rest))
                       (eq (et-type (car rest)) :eclisp-string))
                  (setf documentation (et-value (car rest)))
                  (setf body (cadr rest)))
                 (t
                  (setf body (car rest))))
           (setf (gethash name macro-tbl) (list args body documentation))
           nil)))
  (define-constant +eclisp-macro+
      (intern-eclisp-token "macro"
                           :eclisp-symbol
                           #'eclisp-macro-parse)))

(flet ((eclisp-meta-parse (form ctx)
         (compile-macro (cadr form) (make-hash-table))
         nil))
  (define-constant +eclisp-meta+
      (intern-eclisp-token "meta"
                           :eclisp-symbol
                           #'eclisp-meta-parse)))

(flet ((eclisp-macrofn-parse (form ctx
                                   &aux documentation body)
         (declare (ignore ctx))
         (destructuring-bind (m name args &rest rest) form
           (declare (ignore m))
           (cond ((and (atom (car rest))
                       (eq (et-type (car rest)) :eclisp-string))
                  (setf documentation (car rest))
                  (setf body (cadr rest)))
                 (t
                  (setf body (car rest))))
           (setf (gethash name macrofn-tbl) (list args body documentation))
           nil)))
  (define-constant +eclisp-macrofn+
      (intern-eclisp-token "macrofn"
                           :eclisp-symbol
                           #'eclisp-macrofn-parse)))

;; This is an opaque keyword, whose semantic is fully resolved only
;; at macro expansion time.
(flet ((eclisp-lambda-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-lambda+
      (intern-eclisp-token "lambda" :eclisp-symbol)))

(flet ((eclisp-funcall-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-funcall+
      (intern-eclisp-token "funcall" :eclisp-symbol)))

(flet
    ((eclisp-macrolet-parse (args ctx &aux res tmp-bindings)
       (pop args)
       (destructuring-bind (bindings &rest body) args
         ;; augment the ctx with the let bindings
         (do ((bcur bindings (cdr bcur)))
             ((not bcur))
           (let ((name nil) (tmpl nil) (documentation nil) (ml-body nil))
             (setf name (caar bcur))
             (setf tmpl (cadar bcur))
             (if (eq (et-type (caddar bcur)) :eclisp-string)
                 (progn
                   (setf documentation (caddar bcur))
                   (setf ml-body (car (cdddar bcur))))
                 (setf ml-body (caddar bcur)))
             (setf tmp-bindings (cons (list name (gethash name macro-tbl)) tmp-bindings))
             (setf (gethash name macro-tbl) (list tmpl ml-body))))
         (setf res
               (list* +eclisp-prog*+
                      (loop for bodyform in body collect (eclisp-eval bodyform ctx))))
         ;; remove the bindings
         (do ((bcur tmp-bindings (cdr bcur)))
             ((not bcur))
           (remhash (caar bcur) macro-tbl)
           (when (cadar bcur)
             (setf (gethash (caar bcur) macro-tbl) (cadar bcur)))))
       res))
  (define-constant +eclisp-macrolet+
      (intern-eclisp-token "macrolet"
                           :eclisp-symbol
                           #'eclisp-macrolet-parse)))

(flet ((eclisp-list-eval (args ctx)
         (parse-call args ctx)))
  (define-constant +eclisp-list+
      (intern-eclisp-token "list"
                           :eclisp-symbol
                           #'eclisp-list-eval)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-list*+
      (intern-eclisp-token "list*" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-symbolp+
      (intern-eclisp-token "symbolp" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-listp+
      (intern-eclisp-token "listp" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-car+
      (intern-eclisp-token "car" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-cdr+
      (intern-eclisp-token "cdr" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-null+
      (intern-eclisp-token "null" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-consp+
      (intern-eclisp-token "consp" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-stringp+
      (intern-eclisp-token "stringp" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-length+
      (intern-eclisp-token "length" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-numberp+
      (intern-eclisp-token "numberp" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-concat+
      (intern-eclisp-token "concat" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-symbolicate+
      (intern-eclisp-token "symbolicate" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-gensym+
      (intern-eclisp-token "gensym" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-let+
      (intern-eclisp-token "let" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-dbind+
      (intern-eclisp-token "destructuring-bind" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-append+
      (intern-eclisp-token "append" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-unquote+
      (intern-eclisp-token "unquote" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-unquote-splice+
      (intern-eclisp-token "unquote-splice" :eclisp-symbol)))

(flet ((eclisp-quote-parse (form ctx)
         (declare (ignore ctx))
         form))
  (define-constant +eclisp-quote+
      (intern-eclisp-token "quote" :eclisp-symbol #'eclisp-quote-parse)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-backquote+
      (intern-eclisp-token "backquote" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-error+
      (intern-eclisp-token "error" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-warn+
      (intern-eclisp-token "warn" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-scope-type+
      (intern-eclisp-token "scope-type" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-scope-syms+
      (intern-eclisp-token "scope-symbols" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-scope-sym-get+
      (intern-eclisp-token "scope-symbol-get" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-scope-sym-set+
      (intern-eclisp-token "scope-symbol-set" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-sym-attrs-get+
      (intern-eclisp-token "symbol-attrs-get" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-sym-attrs-set+
      (intern-eclisp-token "symbol-attrs-set" :eclisp-symbol)))

(flet ((eclisp-rshift-parse (form ctx)
         (parse-call form ctx)))
  (define-constant +eclisp-macroexpand+
      (intern-eclisp-token "macroexpand" :eclisp-symbol)))
