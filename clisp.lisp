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

(defun compile-include (include-forms to-stream)
  "Compile an include directive.
syntax: (%include header-list)
where header-list is a list of header names either enclosed in double-quotes
or in angle-brackets."
  (loop for filename in include-forms do
	(format to-stream
		(cond ((symbolp filename) "#include ~a~%")
		      (t "#include \"~a\"~%"))
		filename)))
(defun compile-form (form to-stream)
  "Compile a clisp FROM and write it on TO-STREAM"
  (if (consp form)
      (destructuring-bind (op &rest args) form
        (cond
          ((string= "%include" (string op)) (compile-include args to-stream))
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
