(in-package :eclisp-tests)

(def-suite all-tests
    :description "The master suite of all eclisp tests.")

(in-suite all-tests)

(defun test-eclisp ()
  (run! 'all-tests))

(defun compare-to-ref (result reference)
  "Compare two files RESULT and REFERENCE given as filepath, one line at a time.
If both files are the same returns T, otherwise returns NIL.
At the end the RESULT file is deleted."
  (format t "~a ~a~%" result reference)
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

(defun compile-to-file (eclisp-file res-file)
  "Lower CLISP-FILE to a RES-FILE (a C file)"
  (with-open-file (res-stream res-file :direction :output)
    (with-open-file (eclisp-stream eclisp-file)
      (eclisp::print-eclisp eclisp-stream res-stream)))
  res-file)

(test
 conformance-tests
 "Assert the conformance of the implementation"
(let ((test-names '("if" "dot" "break" "continue" "cast" "addr" "switch"
                    "deref" "for" "label" "goto" "seq" "defstruct" "enum"
                    "while" "do-while" "return" "hello" "arrow" "aref"
                    "function" "set" "types" "defvar" "include" "includes"
                    "cpp-define" "cpp-if" "pre-post")))
 (loop for test-name in test-names do
       (is (compare-to-ref (compile-to-file (format nil "t/~a.eclisp" test-name)
                                            (format nil "t/~a.c.tmp" test-name))
                           (format nil "t/~a.c" test-name))))))
