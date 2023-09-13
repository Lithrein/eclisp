(asdf:defsystem #:clisp
  :name "C Lisp"
  :description "A C Preprocessor based on s-exp"
  :version "0.1.0"
  :maintainer "lithrein <lithrein.site@gmail.com>"
  :author "lithrein"
  :serial t
  :in-order-to ((test-op (test-op "clisp/tests")))
  :build-operation "program-op"
  :build-pathname "clisp"
  :entry-point (uiop:symbol-call :clisp "MAIN")
  :depends-on (:alexandria)
  :components ((:file "package")
               (:file "clisp")))

(asdf:defsystem #:clisp/tests
  :depends-on (:clisp :fiveam)
  :perform (test-op (o s)
            (uiop:symbol-call :clisp-tests :test-clisp))
  :components ((:module "t"
                :serial t
                :components ((:file "package")
                             (:file "main")))))
