(asdf:defsystem #:clisp
  :name "C Lisp"
  :description "A C Preprocessor based on s-exp"
  :version "0.1.0"
  :maintainer "lithrein <lithrein.site@gmail.com>"
  :author "lithrein"
  :serial t
  :build-operation "program-op"
  :build-pathname "clisp"
  :entry-point (uiop:symbol-call :clisp "MAIN")
  :components ((:file "package")
               (:file "clisp")))

