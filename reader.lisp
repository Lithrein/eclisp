(in-package #:eclisp)

(defun parse-list (stream)
  ;; skip the opening parenthesis (
  (read-char stream nil)
  (loop for c = (parse stream)
        while c collect c into terms
        finally (return terms)))

(defun parse-number (stream)
  (let ((res 0) (dec 0) (n 1))
    (loop while (number-p (peek-char nil stream nil))
          do (setf res (+ (- (char-code (read-char stream nil)) 48) (* 10 res))))
    (when (char= #\. (peek-char nil stream nil))
      (progn
        ;; eat the .
        (read-char stream nil)
        (loop while (number-p (peek-char nil stream nil)) do
          (setf dec (+ (- (char-code (read-char stream nil)) 48) (* 10 dec)))
          (setf n (* 10 n)))))
    (if (not (= dec 0))
        (coerce (+ res (/ dec n)) 'float)
        res)))

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
  (intern (loop for c = (peek-char nil stream nil nil)
                while (and c (eql c (peek-char t stream nil nil))
                           (not (char= c #\))))
                collect (read-char stream) into letters
                finally (return (coerce letters 'string)))))

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

(defun parse (stream)
  ;; skip whitespace
  (skip-whitespace stream)
  (let ((cur-char (peek-char nil stream nil nil nil)))
    (cond
      ((null cur-char) nil)
      ((number-p cur-char) (parse-number stream))
      ((char= cur-char #\() (parse-list stream))
      ((char= cur-char #\)) (progn (read-char stream nil) nil))
      ((char= cur-char #\") (parse-string stream))
      ((char= cur-char #\#) (parse-character stream))
      ((char= cur-char #\;) (parse-comment stream))
      (t (parse-symbol stream)))))
      ((char= cur-char #\') (parse-quote stream))
