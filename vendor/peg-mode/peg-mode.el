;;; peg-mode.el --- emacs mode for editing PEG grammar files

;; Copyright (C) 2008  Utz-Uwe Haus <lisp@uuhaus.de>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; This file provides a major mode for editing PEG grammar files.
;; It includes font-lock definitions and commands for
;; controlling indentation, re-indenting by subdivisions.
;;
;; To use, put the following in your .emacs:
;;
;; (autoload 'peg-mode "peg-mode" "Mode for editing PEG grammar files" t)
;;
;; You may also want something like:
;;
;; (setq auto-mode-alist
;;       (append '(("\\.peg$"    . peg-mode))
;;               auto-mode-alist))

;;

;;; Code:
(defconst peg-version "0"
  "`peg-mode' version number.")

(require 'custom)

(defgroup peg nil
  "Support for editing PEG grammar files."
  :group 'languages)

(defcustom peg-mode-hook nil
  "*Hook to be run when `peg-mode' is entered."
  :type 'hook
  :group 'peg)


;; Non-customizable
(defconst peg-comment-re "#"
  "Regexp that starts a comment line.")


;; Utils


;; code


;; major-mode stuff
(defvar peg-mode-abbrev-table nil
  "Abbreviation table used in `peg-mode' buffers.")
(define-abbrev-table 'peg-mode-abbrev-table ())

(defvar peg-mode-syntax-table nil
  "Syntax table used in `peg-mode' buffers.")
(if peg-mode-syntax-table
    nil
  (setq peg-mode-syntax-table (make-syntax-table)))
;; comments: starting at ', ending at end of line
(modify-syntax-entry ?#  "<"   peg-mode-syntax-table)
(modify-syntax-entry ?\n ">"   peg-mode-syntax-table)
;;  quoted strings
(modify-syntax-entry ?\' "\""  peg-mode-syntax-table)
(modify-syntax-entry ?\" "\""  peg-mode-syntax-table)
;; operators
(modify-syntax-entry ?&  "."  peg-mode-syntax-table)
(modify-syntax-entry ?!  "."  peg-mode-syntax-table)
(modify-syntax-entry ?+  "."  peg-mode-syntax-table)
(modify-syntax-entry ?*  "."  peg-mode-syntax-table)
(modify-syntax-entry ?/  "."  peg-mode-syntax-table)
(modify-syntax-entry ?{  "<"  peg-mode-syntax-table)
(modify-syntax-entry ?}  ">"  peg-mode-syntax-table)
(modify-syntax-entry ?\\ "\\" peg-mode-syntax-table)
;;
(modify-syntax-entry ?(  "()"   peg-mode-syntax-table)
(modify-syntax-entry ?[  "|"   peg-mode-syntax-table)
(modify-syntax-entry ?]  "|"   peg-mode-syntax-table)
(modify-syntax-entry ?'  "|"   peg-mode-syntax-table)



;;;###autoload
(defun  peg-mode ()
  "Major mode for editing xrdb config files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table peg-mode-syntax-table)
  (setq major-mode 'peg-mode
        mode-name "peg"
        local-abbrev-table peg-mode-abbrev-table)
  ;(use-local-map peg-mode-map)
  (setq font-lock-defaults '(peg-font-lock-keywords))
  ;; local variables
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'indent-region-function)
  ;; now set their values
  (setq parse-sexp-ignore-comments t
        comment-start-skip "![ \t]*"
	comment-start "! "
        comment-end "")
  (setq indent-region-function 'peg-indent-region
        paragraph-ignore-fill-prefix t
        paragraph-start (concat "^[ \t]*$\\|^[ \t]*[!]\\|" page-delimiter)
        paragraph-separate paragraph-start)
  (run-hooks 'peg-mode-hook))

;; faces and font-locking
(defvar peg-rule-name-face 'peg-rule-name-face
  "Face for a production of a PEG rule's production in a peg grammar file.")


(make-face 'peg-rule-name-face)
;(make-face 'xrdb-option-value-face)

(defun peg-font-lock-mode-hook ()
  (or (face-differs-from-default-p 'peg-rule-name-face)
      (copy-face 'font-lock-keyword-face 'peg-rule-name-face))
  (remove-hook 'font-lock-mode-hook 'peg-font-lock-mode-hook))
(add-hook 'font-lock-mode-hook 'peg-font-lock-mode-hook)

(defvar peg-font-lock-keywords
  (list '("\\([^ \t\n]+\\)[ \t]*<-"
          (1 peg-rule-name-face))
	)
  "Additional expressions to highlight in peg grammar file mode.")
(put 'peg-mode 'font-lock-defaults '(peg-font-lock-keywords))


(provide 'peg-mode)
;;; peg-mode.el ends here