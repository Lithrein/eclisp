(in-package :eclisp)

(define-constant +eclisp-whitespace+
    '(#\Space #\Newline #\Tab #\Page #\Linefeed)
  "eClisp considers spaces (#x20), newlines (#xa), tabs (#x9), form
feed (#xc) and carriage returns (#xd) as whitespace characters.")

(defun skip-whitespace (stream)
  (loop for c = (peek-char nil stream nil nil nil)
        while (and c (member c +eclisp-whitespace+))
        do (read-char stream nil))
  stream)

(defstruct eclisp-readtable content dtable)

(defvar +eclisp-readtable+
  (make-eclisp-readtable :content '()
                         :dtable '())
  "The default eClisp readtable.")

(defun eclisp-set-macro-character (char fn
                                   &optional (rt +eclisp-readtable+))
  "Set character CHAR to trigger the reader macro FN."
  (symbol-macrolet ((content (eclisp-readtable-content rt)))
    (cond ((assoc char content)
           (warn (format nil "Overriding definition of ~S in readtable.~%" char))
           (rplacd (assoc char content) fn))
          (t (push `(,char . ,fn) content)))))

(defun eclisp-get-macro-character (char
                                   &optional (rt +eclisp-readtable+))
  "Return the reader macro associated with character CHAR."
  (cdr (assoc char (eclisp-readtable-content rt))))

(defun eclisp-get-macro-character-list (&optional (rt +eclisp-readtable+))
  (mapcar #'car (eclisp-readtable-content rt)))

(defun eclisp-make-dispatch-macro-character (char
                                             &optional (rt +eclisp-readtable+))
  "Allow CHAR to be used as a dispatch character."
  (symbol-macrolet
      ((content (eclisp-readtable-content rt))
       (dtable (eclisp-readtable-dtable rt)))
    (cond
      ((null (assoc char content))
       (push `(,char . nil) dtable)
       (let ((dispatch-to-char
               (lambda (stream char &aux (subchar (read-char stream nil nil)))
                 (funcall
                  (or (cdr (assoc subchar (cdr (assoc char dtable))))
                      (error (format nil "No dispatch function for ~A"
                                     subchar)))
                  stream subchar))))
         (eclisp-set-macro-character char dispatch-to-char)))
      (t
       (warn (format nil "Character ~S is not a dispatch character.~%"
                     char))))))

(defun eclisp-set-dispatch-macro-character (char subchar fn
                                            &optional (rt +eclisp-readtable+))
  "Set the combination of characters CHAR and SUBCHAR to the reader macro FN."
  (symbol-macrolet
      ((content (eclisp-readtable-content rt))
       (dtable (eclisp-readtable-dtable rt))
       (dtable-slot (assoc char (eclisp-readtable-dtable rt))))
    (cond (dtable-slot
           (rplacd dtable-slot (cons `(,subchar . ,fn) (cdr dtable-slot))))
          (t
           (error (format nil "Character ~S is not a dispatch character.~%"
                          char))))))

(defun eclisp-get-dispatch-macro-character (char subchar
                                            &optional (rt +eclisp-readtable+))
  "Return the reader macro associated with character CHAR and SUBCHAR."
  (let ((dtable (assoc char rt)))
    (if (and dtable (listp dtable))
        (assoc subchar dtable)
        (error (format nil "Character ~S is not a dispatch character.~%"
                       char)))))

(defun eclisp-number-from-string (s)
  "Convert a string to a number returns nil.

A number may be preceded by a plus (+) or minus (-) sign.
A leading 0 denotes an octal number.
A leading 0x denotes a hexadecimal number.
A leading 0b denotes a binary number.
Without prefix numbers are assumed to be decimal.

Floating-point numbers must have a period (.) in their representation
and can be followed by e or E and an exponent."
  (let* ((res 0) (frac 0) (expnt 0) (period-pos 0) (cursor 0)
         (len (1- (length s))) (sign 1) (expt-sign 1)
         (digits-2  '(#\0 #\1))
         (digits-8  `(,@digits-2 #\2 #\3 #\4 #\5 #\6 #\7))
         (digits-10 `(,@digits-8 #\8 #\9))
         (digits-16 `(,@digits-10 #\a #\b #\c #\d #\e #\f))
         digits radix cur)
    (flet ((scan-digits (beg end radix digits s &aux (res 0))
             (loop for i from beg to end
                   for c = (aref s i)
                   while (member c digits) do
                     (setf res (+ (position c digits) (* radix res))
                           cursor i))
             (values res cursor))
           (sign-of (c) (cdr (assoc c '((#\+ . 1) (#\- . -1))))))

      (setf cur (aref s cursor))
      (when (and (> len 1) (member cur '(#\+ #\-)))
        (incf cursor)
        (setf sign (sign-of cur)
              cur (aref s cursor)))


      (if (member cur digits-10)
          (cond ((and (char= cur #\0)
                      (< (1+ cursor) len)
                      (member (aref s (1+ cursor)) digits-8))
                 (setf digits digits-8)
                 (incf cursor))
                ((and (char= cur #\0)
                      (< (1+ cursor) len)
                      (char= (aref s (1+ cursor)) #\x))
                 (setf digits digits-16)
                 (incf cursor 2))
                ((and (char= cur #\0)
                      (< (1+ cursor) len)
                      (char= (aref s (1+ cursor)) #\b))
                 (setf digits digits-2)
                 (incf cursor 2))
                (t (setf digits digits-10))))
      (setf radix (length digits))

      (when (> radix 0)
        (multiple-value-setq (res cursor)
          (scan-digits cursor len radix digits s))

        (cond ((= cursor len) (* sign res))
              (t (char= (aref s (1+ cursor)) #\.)
                 (setf period-pos (1+ cursor))
                 (multiple-value-setq (frac cursor)
                   (scan-digits (+ 2 cursor) len radix digits s))
                 (incf res (/ frac (expt radix (- cursor period-pos))))
                 (cond ((= cursor len) (coerce (* sign res) 'float))
                       (t (member (aref s (+ 1 cursor)) '(#\e #\E))
                          (when (member (aref s (+ 2 cursor)) '(#\+ #\-))
                            (setf expt-sign (sign-of (aref s (+ 2 cursor))))
                            (incf cursor))
                          (multiple-value-setq (expnt cursor)
                            (scan-digits (+ 2 cursor) len radix digits s))
                          (setf res (* res (expt radix (* expt-sign expnt))))
                          (if (= len cursor)
                              (coerce (* sign res) 'float))))))))))

(defun eclisp-read-token (stream char
                          &optional (rt +eclisp-readtable+))
  "Read one word from STREAM and returns it.
A word is the longuest contiguous sequence of non whitespace characters."
  (let* ((tok (loop for c = (peek-char nil stream nil nil)
                    while (and c
                               (eql c (peek-char t stream nil nil))
                               (not (eql c #\)))
                               (not (member c (eclisp-get-macro-character-list rt)))
                               (not (member c +eclisp-whitespace+)))
                    collect (read-char stream) into letters
                    finally (return (coerce (cons char letters) 'string))))
         (tok-as-num (eclisp-number-from-string tok)))
    (if tok-as-num
        (intern-eclisp-token tok-as-num :eclisp-number)
        (intern-eclisp-token tok :eclisp-symbol))))

(defun eclisp-read-list (stream char)
  "Read a list of eClisp forms.
Typically associated to the reader macro #\(."
  (declare (ignore char))
  (loop for c = (eclisp-read stream)
        while c collect c into terms
        finally (return (if terms terms '(nil)))))

(defun eclisp-read-close-curly-brace (stream char)
  "Nothing to do when reading the end-list character.
Typically associated to the read macro #\)."
  (declare (ignore stream) (ignore char)))

(defun eclisp-read-string (stream char &aux char-list)
  "Read a string and returns it as an eClisp token.
Whitespaces such as tabs, linefeed, carriage returns and formfeed are escaped
with \t, \n, \r and \f respectively."
  (declare (ignore char))
  (loop
    with escape = nil
    for c = (read-char stream nil)
    while (and c (or (not (char= c #\")) escape)) do
      (cond
        (escape
         (cond
           ((char= c #\x) (setf char-list (list* #\x #\\ char-list)))
           ((char= c #\t) (setf char-list (list* #\t #\\ char-list)))
           ((char= c #\n) (setf char-list (list* #\n #\\ char-list)))
           ((char= c #\r) (setf char-list (list* #\r #\\ char-list)))
           ((char= c #\f) (setf char-list (list* #\f #\\ char-list)))
           ((char= c #\") (setf char-list (list* #\" #\\ char-list)))
           (t (setf char-list (cons c char-list))))
         (setf escape nil))
        (t
         (if (char= #\\ c)
             (setf escape t)
             (setf char-list (cons c char-list))))))
  (intern-eclisp-token (coerce (reverse char-list) 'string)
                       :eclisp-string))

(defun eclisp-read-comment (stream char)
  "Read a comment and store it as an eClisp token."
  (declare (ignore char))
  (loop for c = (peek-char nil stream nil nil nil)
                               while (and c (char= c #\;))
                               do (read-char stream nil))
  (list
   +eclisp-special-comment+
   (make-instance 'eclisp-token
                  :value (loop for c = (read-char stream nil)
                               while (and c (not (char= c #\Newline)))
                               collect c into letters
                               finally (return (coerce letters 'string)))
                  :type :eclisp-string)))

(defun eclisp-read-quote (stream char)
  "Read a quoted expression."
  (declare (ignore char))
;; (labels
;;     ((process-atom (form)
;;        (if (eq (et-type form) :eclisp-symbol)
;;            (list +eclisp-quote+ form)
;;            form))
;;      (eclisp-quote-propagate (form)
;;       Recursively propagate the quote within the form with respect to the
;;       following rules:
;;         'symbol => 'symbol
;;         'number => number
;;         'string => string
;;         '(a ...) => (list (eclisp-quote-propagate a) ...)
;;            (when form
;;              (cond ((atom form) (process-atom form))
;;                    (t (destructuring-bind (fst &rest snd) form
;;                         (cons (if (atom fst) (process-atom fst)
;;                                   (cons (intern-eclisp-token "list")
;;                                         (eclisp-quote-propagate fst)))
;;                               (eclisp-quote-propagate snd)))))))))
;;  (eclisp-quote-propagate form)
  (list (intern-eclisp-token "quote") (eclisp-read stream)))

(defvar *backquote-context* nil
  "Set to `t' when processing a backquoted form.")

(defun eclisp-read-backquote (stream char)
  (declare (ignore char))
  (labels
      ;; cf. The following functions have been adapted from CLtL2 Appendix C.
      ((bracket (x)
         (cond ((atom x)
                (list +eclisp-list+ (bq-read-aux x)))
               ((eq (car x) +eclisp-unquote+)
                (list +eclisp-list+ (cadr x)))
               ((eq (car x) +eclisp-unquote-splice+)
                (cadr x))
               (t (list +eclisp-list+ (bq-read-aux x)))))
       (bq-read-aux (args)
         (cond ((atom args) (list +eclisp-quote+ args))
               ((eq (car args) +eclisp-backquote+)
                (bq-read-aux (bq-read-aux (cadr args))))
               ((eq (car args) +eclisp-unquote+) (cadr args))
               ((eq (car args) +eclisp-unquote-splice+)
                (error ",@~S after `" (cadr args)))
               (t (do ((p args (cdr p))
                       (q '() (cons (bracket (car p)) q)))
                      ((atom p)
                       (cons +eclisp-append+
                             (nreconc q (list (list +eclisp-quote+ p)))))
                    (when (eq (car p) +eclisp-unquote+)
                      (unless (null (cddr p)) (error "Malformed ,~S" p))
                      (return (cons +eclisp-append+
                                    (nreconc q (list (cadr p))))))
                    (when (eq (car p) +eclisp-unquote-splice+)
                      (error "Dotted ,@~S" p))))))
       (maptree (fn x)
         "Auxiliary function like MAPCAR but has two extra
purposes: (1) it handles dotted lists; (2) it tries to make
the result share with the argument x as much as possible."
         (if (atom x)
             (funcall fn x)
             (let ((a (funcall fn (car x)))
                   (d (maptree fn (cdr x))))
               (if (and (eql a (car x)) (eql d (cdr x)))
                   x
                   (cons a d)))))
       (bq-splicing-frob (x)
         "This predicate is true if X looks like ,@foo"
         (and (consp x)
              (eq (car x) +eclisp-unquote-splice+)))
       (bq-frob (x)
         "This predicate is true if X looks like ,@foo or ,foo."
         (and (consp x)
              (or (eq (car x) +eclisp-unquote+)
                  (eq (car x) +eclisp-unquote-splice+))))
       (bq-simplify (x)
         (if (atom x)
             x
             (let ((x (if (eq (car x) +eclisp-quote+)
                          x
                          (maptree #'bq-simplify x))))
               (if (not (eq (car x) +eclisp-append+))
                   x
                   (bq-simplify-args x)))))
       (bq-simplify-args (x)
         (do ((args (reverse (cdr x)) (cdr args))
              (result
               nil
               (cond ((atom (car args))
                      (bq-attach-append +eclisp-append+ (car args) result))
                     ((and (eq (caar args) +eclisp-list+)
                           (notany #'bq-splicing-frob (cdar args)))
                      (bq-attach-conses (cdar args) result))
                     ((and (eq (caar args) +eclisp-list*+)
                           (notany #'bq-splicing-frob (cdar args)))
                      (bq-attach-conses
                       (reverse (cdr (reverse (cdar args))))
                       (bq-attach-append +eclisp-append+
                                         (car (last (car args)))
                                         result)))
                     ((and (eq (caar args) +eclisp-quote+)
                           (consp (cadar args))
                           (not (bq-frob (cadar args)))
                           (null (cddar args)))
                      (bq-attach-conses (list (list +eclisp-quote+
                                                    (caadar args)))
                                        result))
                     (t (bq-attach-append +eclisp-append+
                                          (car args)
                                          result)))))
             ((null args) result)))
       (null-or-quoted (x)
         (or (null x) (and (consp x) (eq (car x) +eclisp-quote+))))
;;; When BQ-ATTACH-APPEND is called, the OP should be #:BQ-APPEND
;;; or #:BQ-NCONC.  This produces a form (op item result) but
;;; some simplifications are done on the fly:
;;;
;;;  (op '(a b c) '(d e f g)) => '(a b c d e f g)
;;;  (op item 'nil) => item, provided item is not a splicable frob
;;;  (op item 'nil) => (op item), if item is a splicable frob
;;;  (op item (op a b c)) => (op item a b c)
       (bq-attach-append (op item result)
         (cond ((and (null-or-quoted item) (null-or-quoted result))
                (list +eclisp-quote+ (append (cadr item) (cadr result))))
               ((or (null result) (equal result (list +eclisp-quote+ nil)))
                (if (bq-splicing-frob item) (list op item) item))
               ((and (consp result) (eq (car result) op))
                (list* (car result) item (cdr result)))
               (t (list op item result))))
;;; The effect of BQ-ATTACH-CONSES is to produce a form as if by
;;; `(LIST* ,@items ,result) but some simplifications are done
;;; on the fly.
;;;
;;;  (LIST* 'a 'b 'c 'd) => '(a b c . d)
;;;  (LIST* a b c 'nil) => (LIST a b c)
;;;  (LIST* a b c (LIST* d e f g)) => (LIST* a b c d e f g)
;;;  (LIST* a b c (LIST d e f g)) => (LIST a b c d e f g)
       (bq-attach-conses (items result)
         (cond ((and (every #'null-or-quoted items)
                     (null-or-quoted result))
                (list +eclisp-quote+
                      (append (mapcar #'cadr items) (cadr result))))
               ((or (null result) (equal result (list +eclisp-quote+ nil)))
                (cons +eclisp-list+ items))
               ((and (consp result)
                     (or (eq (car result) +eclisp-list+)
                         (eq (car result) +eclisp-list*+)))
                (cons (car result) (append items (cdr result))))
               (t (cons +eclisp-list*+ (append items (list result)))))))
    (let ((*backquote-context* t))
      (bq-simplify (bq-read-aux (eclisp-read stream))))))

(defun eclisp-read-commamacro (stream char)
  (declare (ignore char))
  (unless *backquote-context*
    (error "Comma (,) found outside a backquote."))
  (let ((c (peek-char nil stream nil nil)))
    (cond ((char= #\@ c)
           (read-char stream nil nil)
           (list +eclisp-unquote-splice+ (eclisp-read stream)))
          (t
           (list +eclisp-unquote+ (eclisp-read stream))))))

(defun eclisp-read-character (stream char)
  "Read one character and return it as an eClisp token.
Whitespace characters have multi-character names such as Newline, Tab,
Backspace, Linefeed, Page and Space."
  (declare (ignore char))
  (let ((c (loop for c = (peek-char nil stream nil nil)
                 while (and c
                            (eql c (peek-char t stream nil nil))
                            (not (char= c #\))))
                 collect (read-char stream) into letters
                 finally (return (coerce letters 'string)))))
    (if (or (= 1 (length c))
            (member c '("Newline" "Tab" "Backspace" "Linefeed" "Page" "Space" "Return") :test #'string=))
        (cond ((= 1 (length c)) (intern-eclisp-token c :eclisp-character))
              (t (cond
                   ((string= c "Newline")   (intern-eclisp-token "\\r\\n" :eclisp-string))
                   ((string= c "Tab")       (intern-eclisp-token "\\t" :eclisp-character))
                   ((string= c "Backspace") (intern-eclisp-token "\\b" :eclisp-character))
                   ((string= c "Linefeed")  (intern-eclisp-token "\\n" :eclisp-character))
                   ((string= c "Return")    (intern-eclisp-token "\\r" :eclisp-character))
                   ((string= c "Page")      (intern-eclisp-token "\\f" :eclisp-character))
                   ((string= c "Space")     (intern-eclisp-token " " :eclisp-character))))
        (error (format nil "~s is not a character.~%" c))))))

(defun eclisp-read-verbatim (stream char)
  "Read verbatim C code."
  (declare (ignore char))
  (list
   (intern-eclisp-token "%verbatim" :eclisp-symbol)
    (loop with level = 1
        for c = (peek-char nil stream nil nil)
        do (cond ((char= c #\\) (setf c (read-char stream)))
                 ((char= c #\{) (incf level))
                 ((char= c #\}) (decf level)))
        while (and c (> level 0))
        collect (read-char stream) into letters
        finally (return (progn (read-char stream)
                               (coerce letters 'string))))))

(defun eclisp-reader-init ()
  "Initialize the default readtable."
  (eclisp-set-macro-character #\( #'eclisp-read-list)
  (eclisp-set-macro-character #\) #'eclisp-read-close-curly-brace)
  (eclisp-set-macro-character #\" #'eclisp-read-string)
  (eclisp-set-macro-character #\; #'eclisp-read-comment)

  (eclisp-set-macro-character #\' #'eclisp-read-quote)
  (eclisp-set-macro-character #\` #'eclisp-read-backquote)
  (eclisp-set-macro-character #\, #'eclisp-read-commamacro)

  (eclisp-make-dispatch-macro-character #\#)
  (eclisp-set-dispatch-macro-character #\# #\\ #'eclisp-read-character)
  (eclisp-set-dispatch-macro-character #\# #\{ #'eclisp-read-verbatim))

(defun eclisp-read (stream &optional (rt +eclisp-readtable+))
  "The reader."
  (skip-whitespace stream)
  (let ((content (eclisp-readtable-content rt)))
    (loop for c = (read-char stream nil nil) while c
          return
          (prog1
              (funcall (or (cdr (assoc c content)) #'eclisp-read-token)
                       stream c)
            (skip-whitespace stream)))))
