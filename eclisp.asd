(in-package :asdf)

(asdf:defsystem :eclisp
  :name "C Lisp"
  :description "A C Preprocessor based on s-exp"
  :version "0.1.0"
  :maintainer "lithrein <lithrein.site@gmail.com>"
  :author "lithrein"
  :serial t
  :in-order-to ((test-op (test-op "eclisp/tests")))
  :build-operation "program-op"
  :build-pathname "eclisp"
  :entry-point (uiop:symbol-call :eclisp "MAIN")
  :depends-on (:cl-ppcre)
  :components ((:file "package")
               (:file "eclisp")
               (:file "reader")))

(asdf:defsystem :eclisp/tests
  :depends-on (:eclisp :fiveam)
  :perform (test-op (o s)
            (uiop:symbol-call :eclisp-tests :test-eclisp))
  :components ((:module "t"
                :serial t
                :components ((:file "package")
                             (:file "main")))))
