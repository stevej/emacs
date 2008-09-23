;;; sml-mode.el --- Major mode for editing (Standard) ML

;; Copyright (C) 1999,2000,2004  Stefan Monnier
;; Copyright (C) 1994-1997  Matthew J. Morley
;; Copyright (C) 1989       Lars Bo Nielsen

;; Author: Lars Bo Nielsen
;;      Olin Shivers
;;	Fritz Knabe (?)
;;	Steven Gilmore (?)
;;	Matthew Morley <mjm@scs.leeds.ac.uk> (aka <matthew@verisity.com>)
;;	Matthias Blume <blume@cs.princeton.edu> (aka <blume@kurims.kyoto-u.ac.jp>)
;;      (Stefan Monnier) monnier@cs.yale.edu
;; Maintainer: (Stefan Monnier) monnier+lists/emacs/sml@flint.cs.yale.edu
;; Keywords: SML
;; 1.35
;; 2004/11/23 05:13:59

;; This file is not part of GNU Emacs, but it is distributed under the
;; same conditions.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; HISTORY

;; Still under construction: History obscure, needs a biographer as
;; well as a M-x doctor. Change Log on request.

;; Hacked by Olin Shivers for comint from Lars Bo Nielsen's sml.el.

;; Hacked by Matthew Morley to incorporate Fritz Knabe's hilite and
;; font-lock patterns, some of Steven Gilmore's (reduced) easy-menus,
;; and numerous bugs and bug-fixes.

;;; DESCRIPTION

;; See accompanying info file: sml-mode.info

;;; FOR YOUR .EMACS FILE

;; If sml-mode.el lives in some non-standard directory, you must tell
;; emacs where to get it. This may or may not be necessary:

;; (add-to-list 'load-path "~jones/lib/emacs/")

;; Then to access the commands autoload sml-mode with that command:

;; (load "sml-mode-startup")

;; sml-mode-hook is run whenever a new sml-mode buffer is created.

;; Finally, there are inferior-sml-{mode,load}-hooks -- see comments
;; in sml-proc.el. For much more information consult the mode's *info*
;; tree.

;;; Code:

(eval-when-compile (require 'cl))
(require 'sml-util)
(require 'sml-move)
(require 'sml-defs)
(condition-case nil (require 'skeleton) (error nil))

;;; VARIABLES CONTROLLING INDENTATION

(defcustom sml-indent-level 4
  "*Indentation of blocks in ML (see also `sml-structure-indent')."
  :group 'sml
  :type '(integer))

(defcustom sml-indent-args sml-indent-level
  "*Indentation of args placed on a separate line."
  :group 'sml
  :type '(integer))

;; (defvar sml-indent-align-args t
;;   "*Whether the arguments should be aligned.")

;; (defvar sml-case-indent nil
;;   "*How to indent case-of expressions.
;;     If t:   case expr                     If nil:   case expr of
;;               of exp1 => ...                            exp1 => ...
;;                | exp2 => ...                          | exp2 => ...

;; The first seems to be the standard in SML/NJ, but the second
;; seems nicer...")

(defcustom sml-electric-semi-mode nil
  "*If non-nil, `\;' will self insert, reindent the line, and do a newline.
If nil, just insert a `\;'.  (To insert while t, do: \\[quoted-insert] \;)."
  :group 'sml
  :type 'boolean)

(defcustom sml-rightalign-and t
  "If non-nil, right-align `and' with its leader.
If nil:					If t:
	datatype a = A				datatype a = A
	and b = B				     and b = B"
  :group 'sml
  :type 'boolean)

;;; OTHER GENERIC MODE VARIABLES

(defvar sml-mode-info "sml-mode"
  "*Where to find Info file for `sml-mode'.
The default assumes the info file \"sml-mode.info\" is on Emacs' info
directory path.  If it is not, either put the file on the standard path
or set the variable `sml-mode-info' to the exact location of this file

  (setq sml-mode-info \"/usr/me/lib/info/sml-mode\")

in your .emacs file. You can always set it interactively with the
set-variable command.")

(defvar sml-mode-hook nil
  "*Run upon entering `sml-mode'.
This is a good place to put your preferred key bindings.")

;;; CODE FOR SML-MODE

(defun sml-mode-info ()
  "Command to access the TeXinfo documentation for `sml-mode'.
See doc for the variable `sml-mode-info'."
  (interactive)
  (require 'info)
  (condition-case nil
      (info sml-mode-info)
    (error (progn
             (describe-variable 'sml-mode-info)
             (message "Can't find it... set this variable first!")))))


;;; Autoload functions -- no-doc is another idea cribbed from AucTeX!

(let ((sml-no-doc
       "This function is part of sml-proc, and has not yet been loaded.
Full documentation will be available after autoloading the function."))

  (autoload 'sml-compile	"sml-proc"   sml-no-doc t)
  (autoload 'sml-load-file	"sml-proc"   sml-no-doc t)
  (autoload 'switch-to-sml	"sml-proc"   sml-no-doc t)
  (autoload 'sml-send-region	"sml-proc"   sml-no-doc t)
  (autoload 'sml-send-buffer	"sml-proc"   sml-no-doc t))

;; font-lock setup

(defconst sml-keywords-regexp
  (sml-syms-re "abstraction" "abstype" "and" "andalso" "as" "before" "case"
	       "datatype" "else" "end" "eqtype" "exception" "do" "fn"
	       "fun" "functor" "handle" "if" "in" "include" "infix"
	       "infixr" "let" "local" "nonfix" "of" "op" "open" "orelse"
	       "overload" "raise" "rec" "sharing" "sig" "signature"
	       "struct" "structure" "then" "type" "val" "where" "while"
	       "with" "withtype" "o")
  "A regexp that matches any and all keywords of SML.")

(defconst sml-tyvarseq-re
  "\\(\\('+\\(\\sw\\|\\s_\\)+\\|(\\([,']\\|\\sw\\|\\s_\\|\\s-\\)+)\\)\\s-+\\)?")

;;; Font-lock settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom sml-font-lock-symbols nil
  "Display \\ and -> and such using symbols in fonts.
This may sound like a neat trick, but be extra careful: it changes the
alignment and can thus lead to nasty surprises w.r.t layout.
If t, try to use whichever font is available.  Otherwise you can
set it to a particular font of your preference among `japanese-jisx0208'
and `unicode'."
  :type '(choice (const nil)
	         (const t)
	         (const unicode)
	         (const japanese-jisx0208)))

(defconst sml-font-lock-symbols-alist
  (append
   ;; The symbols can come from a JIS0208 font.
   (and (fboundp 'make-char) (charsetp 'japanese-jisx0208)
	(memq sml-font-lock-symbols '(t japanese-jisx0208))
	(list (cons "fn" (make-char 'japanese-jisx0208 38 75))
	      (cons "andalso" (make-char 'japanese-jisx0208 34 74))
	      (cons "orelse" (make-char 'japanese-jisx0208 34 75))
	      ;; (cons "as" (make-char 'japanese-jisx0208 34 97))
	      (cons "not" (make-char 'japanese-jisx0208 34 76))
	      (cons "div" (make-char 'japanese-jisx0208 33 96))
	      ;; (cons "*" (make-char 'japanese-jisx0208 33 95))
	      (cons "->" (make-char 'japanese-jisx0208 34 42))
	      (cons "=>" (make-char 'japanese-jisx0208 34 77))
	      (cons "<-" (make-char 'japanese-jisx0208 34 43))
	      (cons "<>" (make-char 'japanese-jisx0208 33 98))
	      (cons ">=" (make-char 'japanese-jisx0208 33 102))
	      (cons "<=" (make-char 'japanese-jisx0208 33 101))
	      (cons "..." (make-char 'japanese-jisx0208 33 68))
	      ;; Some greek letters for type parameters.
	      (cons "'a" (make-char 'japanese-jisx0208 38 65))
	      (cons "'b" (make-char 'japanese-jisx0208 38 66))
	      (cons "'c" (make-char 'japanese-jisx0208 38 67))
	      (cons "'d" (make-char 'japanese-jisx0208 38 68))
	      ))
   ;; Or a unicode font.
   (and (fboundp 'decode-char)
	(memq sml-font-lock-symbols '(t unicode))
	(list (cons "fn" (decode-char 'ucs 955))
	      (cons "andalso" (decode-char 'ucs 8896))
	      (cons "orelse" (decode-char 'ucs 8897))
	      ;; (cons "as" (decode-char 'ucs 8801))
	      (cons "not" (decode-char 'ucs 160))
	      (cons "div" (decode-char 'ucs 247))
	      (cons "*" (decode-char 'ucs 215))
	      (cons "o"  (decode-char 'ucs 9675))
	      (cons "->" (decode-char 'ucs 8594))
	      (cons "=>" (decode-char 'ucs 8658))
	      (cons "<-" (decode-char 'ucs 8592))
	      (cons "<>" (decode-char 'ucs 8800))
	      (cons ">=" (decode-char 'ucs 8805))
	      (cons "<=" (decode-char 'ucs 8804))
	      (cons "..." (decode-char 'ucs 8943))
	      ;; (cons "::" (decode-char 'ucs 8759))
	      ;; Some greek letters for type parameters.
	      (cons "'a" (decode-char 'ucs 945))
	      (cons "'b" (decode-char 'ucs 946))
	      (cons "'c" (decode-char 'ucs 947))
	      (cons "'d" (decode-char 'ucs 948))
	      ))))

(defun sml-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((start (match-beginning 0))
	 (end (match-end 0))
	 (syntaxes (if (eq (char-syntax (char-after start)) ?w)
		       '(?w) '(?. ?\\))))
    (if (or (memq (char-syntax (or (char-before start) ?\ )) syntaxes)
	    (memq (char-syntax (or (char-after end) ?\ )) syntaxes)
	    (memq (get-text-property start 'face)
		  '(font-lock-doc-face font-lock-string-face
		    font-lock-comment-face)))
	;; No composition for you.  Let's actually remove any composition
	;; we may have added earlier and which is now incorrect.
	(remove-text-properties start end '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region start end (cdr (assoc (match-string 0) alist)))))
  ;; Return nil because we're not adding any face property.
  nil)

(defun sml-font-lock-symbols-keywords ()
  (when (fboundp 'compose-region)
    (let ((alist nil))
      (dolist (x sml-font-lock-symbols-alist)
	(when (and (if (fboundp 'char-displayable-p)
		       (char-displayable-p (cdr x))
		     t)
		   (not (assoc (car x) alist)))	;Not yet in alist.
	  (push x alist)))
      (when alist
	`((,(regexp-opt (mapcar 'car alist) t)
	   (0 (sml-font-lock-compose-symbol ',alist))))))))

;; The font lock regular expressions.

(defconst sml-font-lock-keywords
  `(;;(sml-font-comments-and-strings)
    (,(concat "\\<\\(fun\\|and\\)\\s-+" sml-tyvarseq-re "\\(\\sw+\\)\\s-+[^ \t\n=]")
     (1 font-lock-keyword-face)
     (6 font-lock-function-name-face))
    (,(concat "\\<\\(\\(data\\|abs\\|with\\|eq\\)?type\\)\\s-+" sml-tyvarseq-re "\\(\\sw+\\)")
     (1 font-lock-keyword-face)
     (7 font-lock-type-def-face))
    ("\\<\\(val\\)\\s-+\\(\\sw+\\>\\s-*\\)?\\(\\sw+\\)\\s-*[=:]"
     (1 font-lock-keyword-face)
     ;;(6 font-lock-variable-def-face nil t)
     (3 font-lock-variable-name-face))
    ("\\<\\(structure\\|functor\\|abstraction\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-module-def-face))
    ("\\<\\(signature\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-interface-def-face))
    
    (,sml-keywords-regexp . font-lock-keyword-face)
    ,@(sml-font-lock-symbols-keywords))
  "Regexps matching standard SML keywords.")

(defface font-lock-type-def-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight type definitions."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-type-def-face 'font-lock-type-def-face
  "Face name to use for type definitions.")

(defface font-lock-module-def-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight module definitions."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-module-def-face 'font-lock-module-def-face
  "Face name to use for module definitions.")

(defface font-lock-interface-def-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight interface definitions."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-interface-def-face 'font-lock-interface-def-face
  "Face name to use for interface definitions.")

;;
;; Code to handle nested comments and unusual string escape sequences
;;

(defsyntax sml-syntax-prop-table
  '((?\\ . ".") (?* . "."))
  "Syntax table for text-properties")

;; For Emacsen that have no built-in support for nested comments
(defun sml-get-depth-st ()
  (save-excursion
    (let* ((disp (if (eq (char-before) ?\)) (progn (backward-char) -1) nil))
	   (_ (backward-char))
	   (disp (if (eq (char-before) ?\() (progn (backward-char) 0) disp))
	   (pt (point)))
      (when disp
	(let* ((depth
		(save-match-data
		  (if (re-search-backward "\\*)\\|(\\*" nil t)
		      (+ (or (get-char-property (point) 'comment-depth) 0)
			 (case (char-after) (?\( 1) (?* 0))
			 disp)
		    0)))
	       (depth (if (> depth 0) depth)))
	  (put-text-property pt (1+ pt) 'comment-depth depth)
	  (when depth sml-syntax-prop-table))))))

(defconst sml-font-lock-syntactic-keywords
  `(("^\\s-*\\(\\\\\\)" (1 ',sml-syntax-prop-table))
    ,@(unless sml-builtin-nested-comments-flag
	'(("(?\\(\\*\\))?" (1 (sml-get-depth-st)))))))

(defconst sml-font-lock-defaults
  '(sml-font-lock-keywords nil nil ((?_ . "w") (?' . "w")) nil
    (font-lock-syntactic-keywords . sml-font-lock-syntactic-keywords)))

;;;;
;;;; Imenu support
;;;;

(defvar sml-imenu-regexp
  (concat "^[ \t]*\\(let[ \t]+\\)?"
	  (regexp-opt (append sml-module-head-syms
			      '("and" "fun" "datatype" "abstype" "type")) t)
	  "\\>"))

(defun sml-imenu-create-index ()
  (let (alist)
    (goto-char (point-max))
    (while (re-search-backward sml-imenu-regexp nil t)
      (save-excursion
	(let ((kind (match-string 2))
	      (column (progn (goto-char (match-beginning 2)) (current-column)))
	      (location
	       (progn (goto-char (match-end 0))
		      (sml-forward-spaces)
		      (when (looking-at sml-tyvarseq-re)
			(goto-char (match-end 0)))
		      (point)))
	      (name (sml-forward-sym)))
	  ;; Eliminate trivial renamings.
	  (when (or (not (member kind '("structure" "signature")))
		    (progn (search-forward "=")
			   (sml-forward-spaces)
			   (looking-at "sig\\|struct")))
	    (push (cons (concat (make-string (/ column 2) ?\ ) name) location)
		  alist)))))
    alist))

;;; MORE CODE FOR SML-MODE

;;;###autoload (add-to-list 'load-path (file-name-directory load-file-name))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.s\\(ml\\|ig\\)\\'" . sml-mode))

;;;###autoload
(define-derived-mode sml-mode fundamental-mode "SML"
  "\\<sml-mode-map>Major mode for editing ML code.
This mode runs `sml-mode-hook' just before exiting.
\\{sml-mode-map}"
  (set (make-local-variable 'font-lock-defaults) sml-font-lock-defaults)
  (set (make-local-variable 'outline-regexp) sml-outline-regexp)
  (set (make-local-variable 'imenu-create-index-function)
       'sml-imenu-create-index)
  (set (make-local-variable 'add-log-current-defun-function)
       'sml-current-fun-name)
  ;; Treat paragraph-separators in comments as paragraph-separators.
  (set (make-local-variable 'paragraph-separate)
       (concat "\\([ \t]*\\*)?\\)?\\(" paragraph-separate "\\)"))
  (set (make-local-variable 'require-final-newline) t)
  ;; forward-sexp-function is an experimental variable in my hacked Emacs.
  (set (make-local-variable 'forward-sexp-function) 'sml-user-forward-sexp)
  ;; For XEmacs
  (easy-menu-add sml-mode-menu)
  ;; Compatibility.  FIXME: we should use `-' in Emacs-CVS.
  (unless (boundp 'skeleton-positions) (set (make-local-variable '@) nil))
  (sml-mode-variables))

(defun sml-mode-variables ()
  (set-syntax-table sml-mode-syntax-table)
  (setq local-abbrev-table sml-mode-abbrev-table)
  ;; A paragraph is separated by blank lines or ^L only.
  
  (set (make-local-variable 'indent-line-function) 'sml-indent-line)
  (set (make-local-variable 'comment-start) "(* ")
  (set (make-local-variable 'comment-end) " *)")
  (set (make-local-variable 'comment-nested) t)
  ;;(set (make-local-variable 'block-comment-start) "* ")
  ;;(set (make-local-variable 'block-comment-end) "")
  ;; (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-start-skip) "(\\*+\\s-*"))

(defun sml-funname-of-and ()
  "Name of the function this `and' defines, or nil if not a function.
Point has to be right after the `and' symbol and is not preserved."
  (sml-forward-spaces)
  (if (looking-at sml-tyvarseq-re) (goto-char (match-end 0)))
  (let ((sym (sml-forward-sym)))
    (sml-forward-spaces)
    (unless (or (member sym '(nil "d="))
		(member (sml-forward-sym) '("d=")))
      sym)))

(defun sml-electric-pipe ()
  "Insert a \"|\".
Depending on the context insert the name of function, a \"=>\" etc."
  (interactive)
  (sml-with-ist
   (unless (save-excursion (skip-chars-backward "\t ") (bolp)) (insert "\n"))
   (insert "| ")
   (let ((text
	  (save-excursion
	    (backward-char 2)		;back over the just inserted "| "
	    (let ((sym (sml-find-matching-starter sml-pipeheads
						  (sml-op-prec "|" 'back))))
	      (sml-forward-sym)
	      (sml-forward-spaces)
	      (cond
	       ((string= sym "|")
		(let ((f (sml-forward-sym)))
		  (sml-find-forward "\\(=>\\|=\\||\\)\\S.")
		  (cond
		   ((looking-at "|") "") ;probably a datatype
		   ((looking-at "=>") " => ") ;`case', or `fn' or `handle'
		   ((looking-at "=") (concat f "  = "))))) ;a function
	       ((string= sym "and")
		;; could be a datatype or a function
		(setq sym (sml-funname-of-and))
		(if sym (concat sym "  = ") ""))
	       ;; trivial cases
	       ((string= sym "fun")
		(while (and (setq sym (sml-forward-sym))
			    (string-match "^'" sym))
		  (sml-forward-spaces))
		(concat sym "  = "))
	       ((member sym '("case" "handle" "fn" "of")) " => ")
	       ;;((member sym '("abstype" "datatype")) "")
	       (t ""))))))

     (insert text)
     (indent-according-to-mode)
     (beginning-of-line)
     (skip-chars-forward "\t |")
     (skip-syntax-forward "w")
     (skip-chars-forward "\t ")
     (when (eq ?= (char-after)) (backward-char)))))

(defun sml-electric-semi ()
  "Insert a \;.
If variable `sml-electric-semi-mode' is t, indent the current line, insert
a newline, and indent."
  (interactive)
  (insert "\;")
  (if sml-electric-semi-mode
      (reindent-then-newline-and-indent)))

;;; INDENTATION !!!

(defun sml-mark-function ()
  "Synonym for `mark-paragraph' -- sorry.
If anyone has a good algorithm for this..."
  (interactive)
  (mark-paragraph))

(defun sml-indent-line ()
  "Indent current line of ML code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
	(indent (max (or (ignore-errors (sml-calculate-indentation)) 0) 0)))
    (if savep
	(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun sml-back-to-outer-indent ()
  "Unindents to the next outer level of indentation."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "\t ")
    (let ((start-column (current-column))
          (indent (current-column)))
      (if (> start-column 0)
          (progn
            (save-excursion
              (while (>= indent start-column)
                (if (re-search-backward "^[^\n]" nil t)
                    (setq indent (current-indentation))
                  (setq indent 0))))
            (backward-delete-char-untabify (- start-column indent)))))))

(defun sml-find-comment-indent ()
  (save-excursion
    (let ((depth 1))
      (while (> depth 0)
	(if (re-search-backward "(\\*\\|\\*)" nil t)
	    (cond
	     ;; FIXME: That's just a stop-gap.
	     ((eq (get-text-property (point) 'face) 'font-lock-string-face))
	     ((looking-at "*)") (incf depth))
	     ((looking-at comment-start-skip) (decf depth)))
	  (setq depth -1)))
      (if (= depth 0)
	  (1+ (current-column))
	nil))))

(defun sml-calculate-indentation ()
  (save-excursion
    (beginning-of-line) (skip-chars-forward "\t ")
    (sml-with-ist
     ;; Indentation for comments alone on a line, matches the
     ;; proper indentation of the next line.
     (when (looking-at "(\\*") (sml-forward-spaces))
     (let (data
	   (sym (save-excursion (sml-forward-sym))))
       (or
	;; Allow the user to override the indentation.
	(when (looking-at (concat ".*" (regexp-quote comment-start)
				  "[ \t]*fixindent[ \t]*"
				  (regexp-quote comment-end)))
	  (current-indentation))

	;; Continued comment.
	(and (looking-at "\\*") (sml-find-comment-indent))

	;; Continued string ? (Added 890113 lbn)
	(and (looking-at "\\\\")
	     (save-excursion
	       (if (save-excursion (previous-line 1)
				   (beginning-of-line)
				   (looking-at "[\t ]*\\\\"))
		   (progn (previous-line 1) (current-indentation))
		 (if (re-search-backward "[^\\\\]\"" nil t)
		     (1+ (current-column))
		   0))))

	;; Closing parens.  Could be handled below with `sml-indent-relative'?
	(and (looking-at "\\s)")
	     (save-excursion
	       (skip-syntax-forward ")")
	       (backward-sexp 1)
	       (if (sml-dangling-sym)
		   (sml-indent-default 'noindent)
		 (current-column))))

	(and (setq data (assoc sym sml-close-paren))
	     (sml-indent-relative sym data))

	(and (member sym sml-starters-syms)
	     (sml-indent-starter sym))

	(and (string= sym "|") (sml-indent-pipe))

	(sml-indent-arg)
	(sml-indent-default))))))

(defsubst sml-bolp ()
  (save-excursion (skip-chars-backward " \t|") (bolp)))

(defun sml-indent-starter (orig-sym)
  "Return the indentation to use for a symbol in `sml-starters-syms'.
Point should be just before the symbol ORIG-SYM and is not preserved."
  (let ((sym (unless (save-excursion (sml-backward-arg))
	       (sml-backward-spaces)
	       (sml-backward-sym))))
    (if (member sym '(";" "d=")) (setq sym nil))
    (if sym (sml-get-sym-indent sym)
      ;; FIXME: this can take a *long* time !!
      (setq sym (sml-find-matching-starter sml-starters-syms))
      ;; Don't align with `and' because it might be specially indented.
      (if (and (or (equal orig-sym "and") (not (equal sym "and")))
	       (sml-bolp))
	  (+ (current-column)
	     (if (and sml-rightalign-and (equal orig-sym "and"))
		 (- (length sym) 3) 0))
	(sml-indent-starter orig-sym)))))

(defun sml-indent-relative (sym data)
  (save-excursion
    (sml-forward-sym) (sml-backward-sexp nil)
    (unless (second data) (sml-backward-spaces) (sml-backward-sym))
    (+ (or (cdr (assoc sym sml-symbol-indent)) 0)
       (sml-delegated-indent))))

(defun sml-indent-pipe ()
  (let ((sym (sml-find-matching-starter sml-pipeheads
					(sml-op-prec "|" 'back))))
    (when sym
      (if (string= sym "|")
	  (if (sml-bolp) (current-column) (sml-indent-pipe))
	(let ((pipe-indent (or (cdr (assoc "|" sml-symbol-indent)) -2)))
	  (when (or (member sym '("datatype" "abstype"))
		    (and (equal sym "and")
			 (save-excursion
			   (forward-word 1)
			   (not (sml-funname-of-and)))))
	    (re-search-forward "="))
	  (sml-forward-sym)
	  (sml-forward-spaces)
	  (+ pipe-indent (current-column)))))))

(defun sml-find-forward (re)
  (sml-forward-spaces)
  (while (and (not (looking-at re))
	      (progn
		(or (ignore-errors (forward-sexp 1) t) (forward-char 1))
		(sml-forward-spaces)
		(not (looking-at re))))))

(defun sml-indent-arg ()
  (and (save-excursion (ignore-errors (sml-forward-arg)))
       ;;(not (looking-at sml-not-arg-re))
       ;; looks like a function or an argument
       (sml-move-if (sml-backward-arg))
       ;; an argument
       (if (save-excursion (not (sml-backward-arg)))
	   ;; a first argument
	   (+ (current-column) sml-indent-args)
	 ;; not a first arg
	 (while (and (/= (current-column) (current-indentation))
		     (sml-move-if (sml-backward-arg))))
	 (unless (save-excursion (sml-backward-arg))
	   ;; all earlier args are on the same line
	   (sml-forward-arg) (sml-forward-spaces))
	 (current-column))))

(defun sml-get-indent (data sym)
  (let (d)
    (cond
     ((not (listp data)) data)
     ((setq d (member sym data)) (cadr d))
     ((and (consp data) (not (stringp (car data)))) (car data))
     (t sml-indent-level))))

(defun sml-dangling-sym ()
  "Non-nil if the symbol after point is dangling.
The symbol can be an SML symbol or an open-paren. \"Dangling\" means that
it is not on its own line but is the last element on that line."
  (save-excursion
    (and (not (sml-bolp))
	 (< (sml-point-after (end-of-line))
	    (sml-point-after (or (sml-forward-sym) (skip-syntax-forward "("))
			     (sml-forward-spaces))))))

(defun sml-delegated-indent ()
  (if (sml-dangling-sym)
      (sml-indent-default 'noindent)
    (sml-move-if (backward-word 1)
		 (looking-at sml-agglomerate-re))
    (current-column)))

(defun sml-get-sym-indent (sym &optional style)
  "Find the indentation for the SYM we're `looking-at'.
If indentation is delegated, point will move to the start of the parent.
Optional argument STYLE is currently ignored."
  (assert (equal sym (save-excursion (sml-forward-sym))))
  (save-excursion
    (let ((delegate (and (not (equal sym "end")) (assoc sym sml-close-paren)))
	  (head-sym sym))
      (when (and delegate (not (eval (third delegate))))
	;;(sml-find-match-backward sym delegate)
	(sml-forward-sym) (sml-backward-sexp nil)
	(setq head-sym
	      (if (second delegate)
		  (save-excursion (sml-forward-sym))
		(sml-backward-spaces) (sml-backward-sym))))

      (let ((idata (assoc head-sym sml-indent-rule)))
	(when idata
	  ;;(if (or style (not delegate))
	  ;; normal indentation
	  (let ((indent (sml-get-indent (cdr idata) sym)))
	    (when indent (+ (sml-delegated-indent) indent)))
	  ;; delgate indentation to the parent
	  ;;(sml-forward-sym) (sml-backward-sexp nil)
	  ;;(let* ((parent-sym (save-excursion (sml-forward-sym)))
	  ;;     (parent-indent (cdr (assoc parent-sym sml-indent-starters))))
	  ;; check the special rules
	  ;;(+ (sml-delegated-indent)
	  ;; (or (sml-get-indent (cdr indent-data) 1 'strict)
	  ;; (sml-get-indent (cdr parent-indent) 1 'strict)
	  ;; (sml-get-indent (cdr indent-data) 0)
	  ;; (sml-get-indent (cdr parent-indent) 0))))))))
	  )))))

(defun sml-indent-default (&optional noindent)
  (let* ((sym-after (save-excursion (sml-forward-sym)))
	 (_ (sml-backward-spaces))
	 (sym-before (sml-backward-sym))
	 (sym-indent (and sym-before (sml-get-sym-indent sym-before)))
	 (indent-after (or (cdr (assoc sym-after sml-symbol-indent)) 0)))
    (when (equal sym-before "end")
      ;; I don't understand what's really happening here, but when
      ;; it's `end' clearly, we need to do something special.
      (forward-word 1)
      (setq sym-before nil sym-indent nil))
    (cond
     (sym-indent
      ;; the previous sym is an indentation introducer: follow the rule
      (if noindent
	  ;;(current-column)
	  sym-indent
	(+ sym-indent indent-after)))
     ;; If we're just after a hanging open paren.
     ((and (eq (char-syntax (preceding-char)) ?\()
	   (save-excursion (backward-char) (sml-dangling-sym)))
      (backward-char)
      (sml-indent-default))
     (t
      ;; default-default
      (let* ((prec-after (sml-op-prec sym-after 'back))
	     (prec (or (sml-op-prec sym-before 'back) prec-after 100)))
	;; go back until you hit a symbol that has a lower prec than the
	;; "current one", or until you backed over a sym that has the same prec
	;; but is at the beginning of a line.
	(while (and (not (sml-bolp))
		    (while (sml-move-if (sml-backward-sexp (1- prec))))
		    (not (sml-bolp)))
	  (while (sml-move-if (sml-backward-sexp prec))))
	(if noindent
	    ;; the `noindent' case does back over an introductory symbol
	    ;; such as `fun', ...
	    (progn
	      (sml-move-if
	       (sml-backward-spaces)
	       (member (sml-backward-sym) sml-starters-syms))
	      (current-column))
	  ;; Use `indent-after' for cases such as when , or ; should be
	  ;; outdented so that their following terms are aligned.
	  (+ (if (progn
		   (if (equal sym-after ";")
		       (sml-move-if
			(sml-backward-spaces)
			(member (sml-backward-sym) sml-starters-syms)))
		   (and sym-after (not (looking-at sym-after))))
		 indent-after 0)
	     (current-column))))))))


;; maybe `|' should be set to word-syntax in our temp syntax table ?
(defun sml-current-indentation ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t|")
    (current-column)))


(defun sml-find-matching-starter (syms &optional prec)
  (let (sym)
    (ignore-errors
      (while
	  (progn (sml-backward-sexp prec)
		 (setq sym (save-excursion (sml-forward-sym)))
		 (not (or (member sym syms) (bobp)))))
      (if (member sym syms) sym))))

(defun sml-skip-siblings ()
  (while (and (not (bobp)) (sml-backward-arg))
    (sml-find-matching-starter sml-starters-syms))
  (when (looking-at "in\\>\\|local\\>")
    ;;skip over `local...in' and continue
    (forward-word 1)
    (sml-backward-sexp nil)
    (sml-skip-siblings)))

(defun sml-beginning-of-defun ()
  (let ((sym (sml-find-matching-starter sml-starters-syms)))
    (if (member sym '("fun" "and" "functor" "signature" "structure"
		      "abstraction" "datatype" "abstype"))
	(save-excursion (sml-forward-sym) (sml-forward-spaces)
			(sml-forward-sym))
      ;; We're inside a "non function declaration": let's skip all other
      ;; declarations that we find at the same level and try again.
      (sml-skip-siblings)
      ;; Obviously, let's not try again if we're at bobp.
      (unless (bobp) (sml-beginning-of-defun)))))

(defcustom sml-max-name-components 3
  "Maximum number of components to use for the current function name."
  :group 'sml
  :type 'integer)

(defun sml-current-fun-name ()
  (save-excursion
    (let ((count sml-max-name-components)
	  fullname name)
      (end-of-line)
      (while (and (> count 0)
		  (setq name (sml-beginning-of-defun)))
	(decf count)
	(setq fullname (if fullname (concat name "." fullname) name))
	;; Skip all other declarations that we find at the same level.
	(sml-skip-siblings))
      fullname)))


;;; INSERTING PROFORMAS (COMMON SML-FORMS)

(defvar sml-forms-alist nil
  "*Alist of code templates.
You can extend this alist to your heart's content.  For each additional
template NAME in the list, declare a keyboard macro or function (or
interactive command) called 'sml-form-NAME'.
If 'sml-form-NAME' is a function it takes no arguments and should
insert the template at point\; if this is a command it may accept any
sensible interactive call arguments\; keyboard macros can't take
arguments at all.  Apropos keyboard macros, see `name-last-kbd-macro'
and `sml-addto-forms-alist'.
`sml-forms-alist' understands let, local, case, abstype, datatype,
signature, structure, and functor by default.")

(defmacro sml-def-skeleton (name interactor &rest elements)
  (when (fboundp 'define-skeleton)
    (let ((fsym (intern (concat "sml-form-" name))))
      `(progn
	 (add-to-list 'sml-forms-alist ',(cons name fsym))
	 (condition-case err
	     ;; Try to use the new `system' flag.
	     (define-abbrev sml-mode-abbrev-table ,name "" ',fsym nil 'system)
	   (wrong-number-of-arguments
	    (define-abbrev sml-mode-abbrev-table ,name "" ',fsym)))
	 (define-skeleton ,fsym
	   ,(format "SML-mode skeleton for `%s..' expressions" name)
	   ,interactor
	   ,(concat name " ") >
	   ,@elements)))))
(put 'sml-def-skeleton 'lisp-indent-function 2)

(sml-def-skeleton "let" nil
  @ "\nin " > _ "\nend" >)

(sml-def-skeleton "if" nil
  @ " then " > _ "\nelse " > _)

(sml-def-skeleton "local" nil
  @ "\nin" > _ "\nend" >)

(sml-def-skeleton "case" "Case expr: "
  str "\nof " > _ " => ")

(sml-def-skeleton "signature" "Signature name: "
  str " =\nsig" > "\n" > _ "\nend" >)

(sml-def-skeleton "structure" "Structure name: "
  str " =\nstruct" > "\n" > _ "\nend" >)

(sml-def-skeleton "functor" "Functor name: "
  str " () : =\nstruct" > "\n" > _ "\nend" >)

(sml-def-skeleton "datatype" "Datatype name and type params: "
  str " =" \n)

(sml-def-skeleton "abstype" "Abstype name and type params: "
  str " =" \n _ "\nwith" > "\nend" >)

;;

(sml-def-skeleton "struct" nil
  _ "\nend" >)

(sml-def-skeleton "sig" nil
  _ "\nend" >)

(sml-def-skeleton "val" nil
  @ " = " > _)

(sml-def-skeleton "fn" nil
  @ " =>" > _)

(sml-def-skeleton "fun" nil
  @ " =" > _)

;;

(defun sml-forms-menu (menu)
  (mapcar (lambda (x) (vector (car x) (cdr x) t))
	  sml-forms-alist))

(defvar sml-last-form "let")

(defun sml-electric-space ()
  "Expand a symbol into an SML form, or just insert a space.
If the point directly precedes a symbol for which an SML form exists,
the corresponding form is inserted."
  (interactive)
  (let ((abbrev-mode (not abbrev-mode))
	(last-command-char ?\ )
	;; Bind `this-command' to fool skeleton's special abbrev handling.
	(this-command 'self-insert-command))
    (call-interactively 'self-insert-command)))

(defun sml-insert-form (name newline)
  "Interactive short-cut to insert the NAME common ML form.
If a prefix argument is given insert a NEWLINE and indent first, or
just move to the proper indentation if the line is blank\; otherwise
insert at point (which forces indentation to current column).

The default form to insert is 'whatever you inserted last time'
\(just hit return when prompted\)\; otherwise the command reads with
completion from `sml-forms-alist'."
  (interactive
   (list (completing-read
	  (format "Form to insert: (default %s) " sml-last-form)
	  sml-forms-alist nil t nil)
	 current-prefix-arg))
  ;; default is whatever the last insert was...
  (if (string= name "") (setq name sml-last-form) (setq sml-last-form name))
  (unless (or (not newline)
	      (save-excursion (beginning-of-line) (looking-at "\\s-*$")))
    (insert "\n"))
  (unless (/= ?w (char-syntax (preceding-char))) (insert " "))
  (let ((f (cdr (assoc name sml-forms-alist))))
    (cond
     ((commandp f) (command-execute f))
     (f (funcall f))
     (t (error "Undefined form: %s" name)))))

;; See also macros.el in emacs lisp dir.

(defun sml-addto-forms-alist (name)
  "Assign a name to the last keyboard macro defined.
Argument NAME is transmogrified to sml-form-NAME which is the symbol
actually defined.

The symbol's function definition becomes the keyboard macro string.

If that works, NAME is added to `sml-forms-alist' so you'll be able to
reinvoke the macro through \\[sml-insert-form].  You might want to save
the macro to use in a later editing session -- see `insert-kbd-macro'
and add these macros to your .emacs file.

See also `edit-kbd-macro' which is bound to \\[edit-kbd-macro]."
  (interactive "sName for last kbd macro (\"sml-form-\" will be added): ")
  (when (string= name "") (error "No command name given"))
  (let ((fsym (intern (concat "sml-form-" name))))
    (name-last-kbd-macro fsym)
    (message "Macro bound to %s" fsym)
    (add-to-list 'sml-forms-alist (cons name fsym))))

;;;;
;;;;  SML/NJ's Compilation Manager support
;;;;

(defvar sml-cm-mode-syntax-table sml-mode-syntax-table)
(defvar sml-cm-font-lock-keywords
 `(,(concat "\\<" (regexp-opt '("library" "group" "is" "structure"
				"functor" "signature" "funsig") t)
	    "\\>")))
;;;###autoload
(add-to-list 'completion-ignored-extensions "CM/")
(add-to-list 'completion-ignored-extensions ".cm/")
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cm\\'" . sml-cm-mode))
;;;###autoload
(define-derived-mode sml-cm-mode fundamental-mode "SML-CM"
  "Major mode for SML/NJ's Compilation Manager configuration files."
  (local-set-key "\C-c\C-c" 'sml-compile)
  (set (make-local-variable 'font-lock-defaults)
       '(sml-cm-font-lock-keywords nil t nil nil)))

;;;;
;;;; ML-Lex support
;;;;

(defvar sml-lex-font-lock-keywords
  (append
   '(("^%\\sw+" . font-lock-builtin-face)
     ("^%%" . font-lock-module-def-face))
   sml-font-lock-keywords))
(defconst sml-lex-font-lock-defaults
  (cons 'sml-lex-font-lock-keywords (cdr sml-font-lock-defaults)))

;;;###autoload
(define-derived-mode sml-lex-mode sml-mode "SML-Lex"
  "Major Mode for editing ML-Lex files."
  (set (make-local-variable 'font-lock-defaults) sml-lex-font-lock-defaults))

;;;;
;;;; ML-Yacc support
;;;;

(defface sml-yacc-bnf-face
  '((t (:foreground "darkgreen")))
  "Face used to highlight (non)terminals in `sml-yacc-mode'.")
(defvar sml-yacc-bnf-face 'sml-yacc-bnf-face)

(defcustom sml-yacc-indent-action 16
  "Indentation column of the opening paren of actions."
  :group 'sml
  :type 'integer)

(defcustom sml-yacc-indent-pipe nil
  "Indentation column of the pipe char in the BNF.
If nil, align it with `:' or with previous cases."
  :group 'sml
  :type 'integer)

(defcustom sml-yacc-indent-term nil
  "Indentation column of the (non)term part.
If nil, align it with previous cases."
  :group 'sml
  :type 'integer)

(defvar sml-yacc-font-lock-keywords
  (cons '("^\\(\\sw+\\s-*:\\|\\s-*|\\)\\(\\s-*\\sw+\\)*\\s-*\\(\\(%\\sw+\\)\\s-+\\sw+\\|\\)"
	  (0 (save-excursion
	       (save-match-data
		 (goto-char (match-beginning 0))
		 (unless (or (re-search-forward "\\<of\\>" (match-end 0) 'move)
			     (progn (sml-forward-spaces)
				    (not (looking-at "("))))
		   sml-yacc-bnf-face))))
	  (4 font-lock-builtin-face t t))
	sml-lex-font-lock-keywords))
(defconst sml-yacc-font-lock-defaults
  (cons 'sml-yacc-font-lock-keywords (cdr sml-font-lock-defaults)))

(defun sml-yacc-indent-line ()
  "Indent current line of ML-Yacc code."
  (let ((savep (> (current-column) (current-indentation)))
	(indent (max (or (ignore-errors (sml-yacc-indentation)) 0) 0)))
    (if savep
	(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun sml-yacc-indentation ()
  (save-excursion
    (back-to-indentation)
    (or (and (looking-at "%\\|\\(\\sw\\|\\s_\\)+\\s-*:") 0)
	(when (save-excursion
		(condition-case nil (progn (up-list -1) nil) (scan-error t)))
	  ;; We're outside an action.
	  (cond
	   ;; Special handling of indentation inside %term and %nonterm
	   ((save-excursion
	      (and (re-search-backward "^%\\(\\sw+\\)" nil t)
		   (member (match-string 1) '("term" "nonterm"))))
	    (if (numberp sml-yacc-indent-term) sml-yacc-indent-term
	      (let ((offset (if (looking-at "|") -2 0)))
		(forward-line -1)
		(looking-at "\\s-*\\(%\\sw*\\||\\)?\\s-*")
		(goto-char (match-end 0))
		(+ offset (current-column)))))
	   ((looking-at "(") sml-yacc-indent-action)
	   ((looking-at "|")
	    (if (numberp sml-yacc-indent-pipe) sml-yacc-indent-pipe
	      (backward-sexp 1)
	      (while (progn (sml-backward-spaces)
			    (/= 0 (skip-syntax-backward "w_"))))
	      (sml-backward-spaces)
	      (if (not (looking-at "\\s-$"))
		  (1- (current-column))
		(skip-syntax-forward " ")
		(- (current-column) 2))))))
	;; default to SML rules
	(sml-calculate-indentation))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.grm\\'" . sml-yacc-mode))
;;;###autoload
(define-derived-mode sml-yacc-mode sml-mode "SML-Yacc"
  "Major Mode for editing ML-Yacc files."
  (set (make-local-variable 'indent-line-function) 'sml-yacc-indent-line)
  (set (make-local-variable 'font-lock-defaults) sml-yacc-font-lock-defaults))

(provide 'sml-mode)

;;; sml-mode.el ends here
