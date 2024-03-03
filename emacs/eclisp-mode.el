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

(defconst eclisp-keywords
  '("aref" "auto" "addr" "break" "cast" "continue" "def" "default"
    "deref" "do" "else" "enum" "extern" "for" "goto" "if" "macro"
    "progn" "return" "sizeof" "switch" "short" "while"))

(defconst eclisp-typ-keywords
  '("array" "char" "const" "double" "float" "int" "long" "ptr"
    "register" "signed" "static" "struct" "typedef" "union"
    "unisgned" "void" "volatile"))

(defconst eclisp-builtins
  '("=" "&=" "+=" "&&" "||" "<=" "->" "+" "-" "*" "/" "."))

(defconst eclisp-keywords-regexp (regexp-opt eclisp-keywords 'words))
(defconst eclisp-typ-keywords-regexp (regexp-opt eclisp-typ-keywords 'words))
(defconst eclisp-builtins-regexp (regexp-opt eclisp-builtins 'symbols))

(defconst eclisp-font-lock-keywords
  `((,";.*$"                              . font-lock-comment-face)
    (,eclisp-keywords-regexp              . font-lock-keyword-face)
    (,eclisp-typ-keywords-regexp          . font-lock-type-face)
    (,eclisp-preprocessor-keywords-regexp . font-lock-preprocessor-face)
    (,"#include"                          . font-lock-preprocessor-face)
    (,"#define"                           . font-lock-preprocessor-face)
    (,"#if"                               . font-lock-preprocessor-face)
    (,eclisp-builtins-regexp              . font-lock-builtin-face)
    (,"\\<[0-9]+\\>"                      . font-lock-constant-face)
    (,"\<[^ ]+\>"                         . font-lock-string-face)
    ))

(define-derived-mode eclisp-mode lisp-mode "eclisp"
  (setq font-lock-defaults '(eclisp-font-lock-keywords))
  (setq mode-name "eclisp"))

(provide 'eclisp-mode)
;;; eclisp-mode.el ends here
