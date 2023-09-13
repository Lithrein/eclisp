(in-package #:clisp-tests)

(def-suite all-tests
    :description "The master suite of all clisp tests.")

(in-suite all-tests)

(defun test-clisp ()
  (run! 'all-tests))

(defun compare-to-ref (result reference)
  "Compare two files RESULT and REFERENCE given as filepath, one line at a time.
If both files are the same returns T, otherwise returns NIL.
At the end the RESULT file is deleted."
  (format t "~a ~a ~%" result reference)
  (let ((res nil))
    (with-open-file (res-stream result)
      (with-open-file (ref-stream reference)
        (do ((res-line (read-line res-stream nil) (read-line res-stream nil))
             (ref-line (read-line ref-stream nil) (read-line ref-stream nil))
             (stop nil)) (stop)
          (if (or (null res-line)
                  (null ref-line)
                  (not (equal res-line ref-line)))
              (progn
                (setf stop t)
                (setf res (equal res-line ref-line)))))))
    (delete-file result)
    res))

(defun compile-to-file (clisp-file res-file)
  "Lower CLISP-FILE to a RES-FILE (a C file)"
  (with-open-file (res-stream res-file :direction :output)
    (with-open-file (clisp-stream clisp-file)
      (clisp::compile-clisp clisp-stream res-stream)))
  res-file)

(test conformance-tests
  "Assert the conformance of the implementation"
  (is (compare-to-ref (compile-to-file "t/include.clisp" "t/include.c.tmp")
                     "t/include.c")))
