(in-package #:eclisp)

(defun parse-list (stream)
  ;; skip the opening parenthesis (
  (read-char stream nil)
  (loop for c = (parse stream)
        while c collect c into terms
        finally (return terms)))

(defun parse-string (stream)
  (let ((escape nil)
        (char-list nil))
    ;; skip the opening "
    (read-char stream nil)
    (loop for c = (read-char stream nil)
          while (and c (or (not (char= c #\")) escape)) do
            (if escape
                (progn
                  (cond ((char= c #\n) (setf char-list (cons #\n (cons #\\ char-list))))
                        ((char= c #\r) (setf char-list (cons #\r (cons #\\ char-list))))
                        ((char= c #\f) (setf char-list (cons #\f (cons #\\ char-list))))
                        (t (setf char-list (cons c char-list))))
                  (setf escape nil))
                (progn
                  (if (char= #\\ c)
                      (setf escape t)
                      (setf char-list (cons c char-list))))))
    (coerce (reverse char-list) 'string)))

(defun parse-symbol (stream)
  (loop for c = (peek-char nil stream nil nil)
        while (and c (eql c (peek-char t stream nil nil))
                   (not (char= c #\))))
        collect (read-char stream) into letters
        finally (return (coerce letters 'string))))

(defun parse-comment (stream)
  ;; skip semi-colons;
  (loop for c = (peek-char nil stream nil nil nil)
        while (and c (char= c #\;))
        do (read-char stream nil))
  `(|%comment|
    ,(loop for c = (read-char stream nil)
          while (and c (not (char= c #\Newline)))
          collect c into letters
          finally (return (coerce letters 'string)))))

(defun skip-whitespace (stream)
  (loop for c = (peek-char nil stream nil nil nil)
        while (and c (or (char= #\Space c)
                         (char= #\Newline c)
                         (char= #\Tab c)))
        do (read-char stream nil))
  stream)

(defun number-p (c)
  (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(defun parse-character (stream)
  ;; skip the #
  (read-char stream nil)
  ;; skip the \
  (read-char stream nil)
  (let ((c (loop for c = (peek-char nil stream nil nil)
                 while (and c (eql c (peek-char t stream nil nil))
                            (not (char= c #\))))
                 collect (read-char stream) into letters
                 finally (return (coerce letters 'string)))))
    (cond
      ((string= c "Newline") (intern "'\\n'"))
      ((string= c "Tab") (intern "'\\t'"))
      ((string= c "Backspace") (intern "'\\b'"))
      ((string= c "Linefeed") (intern "'\\r'"))
      ((string= c "Page") (intern "'\\f'"))
      ((string= c "Space") (intern "' '"))
      (t (intern (concatenate 'string "'" c "'"))))))

(defun parse-quote (stream)
  ;; skip the ' character
  (read-char stream nil)
  `(|quote| ,(parse stream)))

(defun parse-backquote (stream)
  ;; skip the ' character
  (read-char stream nil)
  `(|backquote| ,(parse stream)))

(defun parse-comma (stream)
  ;; skip the , character
  (read-char stream nil)
  (let ((cur-char (peek-char nil stream nil nil nil)))
    (if (char= cur-char #\@)
        (progn
          (read-char stream nil)
          `(|unquote-splice| ,(parse stream)))
        `(|unquote| ,(parse stream)))))

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

(defun parse-dot (stream)
  ;; skip the . character
  (read-char stream nil)
  (let ((c (peek-char nil stream nil nil nil)))
    (if (or (char= #\Space c)
            (char= #\Newline c)
            (char= #\Tab c))
        '|.|
        `(|%key| id ,(parse stream)))))

(defun parse-verbatim (stream)
  (let ((level 1))
    (loop for c = (peek-char nil stream nil nil)
          do (cond ((char= c #\() (incf level))
                   ((char= c #\)) (decf level)))
          while (and c (> level 0))
          if (char= c #\Newline) collect #\\ into letters
          collect (read-char stream) into letters
          finally (return (coerce letters 'string)))))

(defun parse-percent (stream)
  ;; skip the % character
  (read-char stream nil)
  (let ((c (peek-char nil stream nil nil nil)))
    (cond ((char= c #\:)
           (read-char stream nil)
           (list "%:" (parse-verbatim stream)))
          ((char= c #\Space) "%")
          (t (intern (concatenate 'string "%" (parse-symbol stream)))))))

(defun parse (stream)
  ;; skip whitespace
  (skip-whitespace stream)
  (let ((cur-char (peek-char nil stream nil nil nil)))
    (cond
      ((null cur-char) nil)
      ((char= cur-char #\() (parse-list stream))
      ((char= cur-char #\)) (progn (read-char stream nil) nil))
      ((char= cur-char #\%) (parse-percent stream))
      ((char= cur-char #\") (parse-string stream))
      ((char= cur-char #\#) (parse-character stream))
      ((char= cur-char #\;) (parse-comment stream))
      ((char= cur-char #\`) (parse-backquote stream))
      ((char= cur-char #\') (parse-quote stream))
      ((char= cur-char #\,) (parse-comma stream))
      ((char= cur-char #\.) (parse-dot stream))
      ((char= cur-char #\:) (progn (read-char stream nil) `(|%key| num ,(parse stream))))
      (t (with-custom-reader "'|?" (read stream nil))))))
