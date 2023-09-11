(in-package #:clisp)

(defmacro with-case-sensitivity (&body body)
  "Enable case sensitivity while performing a READ."
  `(let ((*readtable* (copy-readtable)))
     (setf (readtable-case *readtable*) :preserve)
     (progn
       ,@body)))

(defun main (from-stream to-stream)
  "Write the result of the compilation of the content of FROM-STREAM into
TO-STREAM."
  (let ((form (with-case-sensitivity (read from-stream))))
    (cond ((equal (string (car form)) "%include")
	   (loop for filename in (cdr form) do
		 (format to-stream
			 (cond ((symbolp filename) "#include ~a~%")
			       (t "#include \"~a\"~%"))
			 filename))))))
