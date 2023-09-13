(in-package #:clisp)

(defmacro with-case-sensitivity (&body body)
  "Enable case sensitivity while performing a READ."
  `(let ((*readtable* (copy-readtable)))
     (setf (readtable-case *readtable*) :preserve)
     (progn
       ,@body)))

(defun compile-include (include-forms to-stream)
  "Compile an include directive."
  (loop for filename in include-forms do
	(format to-stream
		(cond ((symbolp filename) "#include ~a~%")
		      (t "#include \"~a\"~%"))
		filename)))

(defun compile-clisp (from-stream to-stream)
  "Write the result of the compilation of the content of FROM-STREAM into
TO-STREAM."
  (loop for form = (with-case-sensitivity (read from-stream nil))
        while form do
        (switch ((string (car form)) :test #'equal)
                ("%include" (compile-include (cdr form) to-stream))
                (otherwise (format t "unknown construct ~a" (car form))))))

(defun main ()
  (compile-clisp *standard-input* *standard-output*))
