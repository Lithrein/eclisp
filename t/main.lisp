(in-package #:eclisp-tests)

(def-suite all-tests
    :description "The master suite of all eclisp tests.")

(in-suite all-tests)

(defun test-eclisp ()
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

(defun compile-to-file (eclisp-file res-file)
  "Lower CLISP-FILE to a RES-FILE (a C file)"
  (with-open-file (res-stream res-file :direction :output)
    (with-open-file (eclisp-stream eclisp-file)
      (eclisp::compile-eclisp eclisp-stream res-stream)))
  res-file)

(test
 conformance-tests
 "Assert the conformance of the implementation"
 (is (compare-to-ref (compile-to-file "t/if.eclisp"
                                      "t/if.c.tmp")
                     "t/if.c"))
 (is (compare-to-ref (compile-to-file "t/dot.eclisp"
                                      "t/dot.c.tmp")
                     "t/dot.c"))
 (is (compare-to-ref (compile-to-file "t/break.eclisp"
                                      "t/break.c.tmp")
                     "t/break.c"))
 (is (compare-to-ref (compile-to-file "t/continue.eclisp"
                                      "t/continue.c.tmp")
                     "t/continue.c"))
 (is (compare-to-ref (compile-to-file "t/cast.eclisp"
                                      "t/cast.c.tmp")
                     "t/cast.c"))
 (is (compare-to-ref (compile-to-file "t/addr.eclisp"
                                      "t/addr.c.tmp")
                     "t/addr.c"))
 (is (compare-to-ref (compile-to-file "t/deref.eclisp"
                                      "t/deref.c.tmp")
                     "t/deref.c"))
 (is (compare-to-ref (compile-to-file "t/for.eclisp"
                                      "t/for.c.tmp")
                     "t/for.c"))
 (is (compare-to-ref (compile-to-file "t/while.eclisp"
                                      "t/while.c.tmp")
                     "t/while.c"))
 (is (compare-to-ref (compile-to-file "t/do-while.eclisp"
                                      "t/do-while.c.tmp")
                     "t/do-while.c"))
 (is (compare-to-ref (compile-to-file "t/return.eclisp"
                                      "t/return.c.tmp")
                     "t/return.c"))
 (is (compare-to-ref (compile-to-file "t/hello.eclisp"
                                      "t/hello.c.tmp")
                     "t/hello.c"))
 (is (compare-to-ref (compile-to-file "t/arrow.eclisp"
                                      "t/arrow.c.tmp")
                     "t/arrow.c"))
 (is (compare-to-ref (compile-to-file "t/aref.eclisp"
                                      "t/aref.c.tmp")
                     "t/aref.c"))
 (is (compare-to-ref (compile-to-file "t/function.eclisp"
                                      "t/function.c.tmp")
                     "t/function.c"))
 (is (compare-to-ref (compile-to-file "t/set.eclisp"
                                      "t/set.c.tmp")
                     "t/set.c"))
 (is (compare-to-ref (compile-to-file "t/types.eclisp"
                                      "t/types.c.tmp")
                     "t/types.c"))
 (is (compare-to-ref (compile-to-file "t/defvar.eclisp"
                                      "t/defvar.c.tmp")
                     "t/defvar.c"))
 (is (compare-to-ref (compile-to-file "t/include.eclisp"
				      "t/include.c.tmp")
		     "t/include.c"))
 (is (compare-to-ref (compile-to-file "t/includes.eclisp"
				      "t/includes.c.tmp")
		     "t/include.c"))
 (is (compare-to-ref (compile-to-file "t/cpp-define.eclisp"
                                      "t/cpp-define.c.tmp")
		     "t/cpp-define.c"))
 (is (compare-to-ref (compile-to-file "t/cpp-if.eclisp"
                                      "t/cpp-if.c.tmp")
                     "t/cpp-if.c")))
