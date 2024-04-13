(defpackage :eclisp
  (:use :cl)
  (:import-from :cl-ppcre :regex-replace-all)
  (:export "main"))

(in-package :eclisp)

;; Work around sbcl's defconstant
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when doc (list doc))))
