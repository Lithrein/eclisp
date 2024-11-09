;;; eclisp-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Lithrein

;; Author: Lithrein
;; Keywords: languages, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Emacs mode providing syntax highlighting for eClisp files.

;;; Code:

(provide 'eclisp-mode)  

(defconst eclisp-keywords-regexp
  (regexp-opt
   '("aref" "auto" "addr" "break" "cast" "continue" "def" "default"
     "deref" "do" "else" "enum" "extern" "for" "goto" "if" "macro"
     "prog" "prog*" "return" "sizeof" "switch" "short" "while")
   'words))

(defconst eclisp-typ-keywords-regexp
  (regexp-opt
   '("array" "char" "const" "double" "float" "int" "long" "ptr"
     "register" "signed" "static" "struct" "typedef" "union"
     "unisgned" "void" "volatile")  
   'words))

(defconst eclisp-operators-regexp
  (regexp-opt
   '("=" "&=" "+=" "&&" "||" "<=" "->" "+" "-" "*" "/" ".")
   'symbols))

(defconst eclisp-pp-keywords-regexp
  (regexp-opt
   '("%:include" "%:define" "%:if")
   'symbols))

(defconst eclisp-font-lock-keywords
  `((,";.*$"                              . font-lock-comment-face)
    (,eclisp-operators-regexp             . font-lock-builtin-face)
    (,eclisp-keywords-regexp              . font-lock-keyword-face)
    (,"&\\(body\\|rest\\|keys\\)"         . font-lock-keyword-face)
    (,eclisp-typ-keywords-regexp          . font-lock-type-face)
    (,"\\<[0-9]+\\>"                      . font-lock-constant-face)
    (,eclisp-pp-keywords-regexp           . font-lock-preprocessor-face)
    (,"\<[^ ]+\>"                         . font-lock-string-face)
    (,"[\\\.\|:][a-zA-z_][a-zA-Z0-9_]*"   . font-lock-preprocessor-face)))

(define-derived-mode eclisp-mode lisp-mode "eclisp"
  (setq font-lock-defaults '(eclisp-font-lock-keywords))
  (setq mode-name "eclisp"))

(provide 'eclisp-mode)
;;; eclisp-mode.el ends here
