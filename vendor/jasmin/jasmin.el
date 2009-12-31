;; jasmin.el -- major editing mode for Jasmin Java bytecode assembler files

;; Copyright (C) 1996,2002 Neil W. Van Dyke

;; Author:    Neil W. Van Dyke <neil@neilvandyke.org>
;; Version:   1.2
;; X-URL:     http://www.neilvandyke.org/jasmin-emacs/
;; X-CVS:     $Id: jasmin.el,v 1.69 2002/10/16 00:36:59 nwv Exp $

;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.  This
;; is distributed in the hope that it will be useful, but without any warranty;
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose.  See the GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file `COPYING'.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.

;;; Change Log:

;; [Version 1.3 30-Dec-2009] (stevej) Added support for the implements keyword.

;; [Version 1.2, 15-Oct-2002] Changed contact info again.  Disabled bug report
;; function.  Comment changes.

;; [Version 1.1, 18-Mar-2002] Changed contact info.

;; [Version 1.0, 15-Aug-1996] Initial release.  Indenting, fontifying, and
;; quick-ref work, so it's being made available to anyone who can use it.  The
;; next release will included fixes for reported bugs and a couple minor
;; feature additions.

;;; Commentary:

;; THIS PACKAGE IS NO LONGER BEING MAINTAINED.
;;
;; `jasmin.el' is an Emacs major mode for editing Jasmin Java bytecode
;; assembler files.
;;
;; Major features are:
;;
;;   * Indenting, with a little non-indenting formatting thrown in.
;;
;;   * Font-lock fontifying.  (If you find this to be too slow, set
;;     `jasmin-fontify-instructions' to nil.)
;;
;;   * Quick-reference help on instructions and directives.

;; The absolute easiest way to install this if you're not yet an Emacs weenie
;; is to copy this file to some directory (say, `/home/joebob/emacs/'), and put
;; something like the following in your `.emacs' file:
;;
;;     (load "/home/joebob/emacs/jasmin.el")
;;
;; As you might suspect, the easiest way isn't the best way, but this'll get
;; you by if you're in a hurry.

;; This package was tested with GNU Emacs 19.31, and should work with any
;; recent version.  It'll probably work with the prodigal XEmacs too.

;; Used as references were Jonathan Meyer's Jasmin documentation (Jul 1996,
;; `http://found.cs.nyu.edu/meyer/jasmin/'), and a draft JavaSoft "Java Virtual
;; Machine Specification" (1.0 Beta, 22 Aug 1995,
;; `http://www.javasoft.com/doc/language_vm_specification.html').  At this
;; writing, the Java VM book Meyer co-authored with Troy Downing
;; (O'Reilly&Associates, ISBN 1-56592-194-1) has not yet been published.  Sun
;; also has a Java VM book due out any day now (Sun/Addison-Wesley).

;;; Code:

(require 'faces)
(require 'font-lock)

(defconst jasmin-version "1.3")

(defconst jasmin-author "Neil W. Van Dyke <neil@neilvandyke.org>")

(defconst jasmin-maintainer-address "stevej@pobox.com")

(defconst jasmin-vc-id
  "$Id: jasmin.el,v 1.69 2002/10/16 00:36:59 nwv Exp $")

;; Option Variables:

(defvar jasmin-edit-auto-mode-alist t
  "*If non-nil, edits `auto-mode-alist' when package is loaded.")

(defvar jasmin-help-window-size 3
  "*Height of help window, including mode line.")

(defvar jasmin-i-hate-tabs t
  "*If non-nil, sets `indent-tabs-mode' to nil for each buffer.")

(defvar jasmin-fontify-instructions t
  "*If non-nil, instructions are fontified along with the other constructs.
Note that this can be slow, so you may wish to turn it off.")

(defvar jasmin-turn-on-font-lock window-system
  "*If non-nil, then font-lock is turned on during mode initialization.")

(defvar jasmin-global-directive-indent 0
  "*Indentation for global directive lines.")

(defvar jasmin-instruction-indent 4
  "*Indentation for instruction lines.")

(defvar jasmin-label-indent 0
  "*Indentation for label lines.")

(defvar jasmin-method-directive-indent 4
  "*Indentation for method-local directive lines.")

(defvar jasmin-tableswitch-case-indent 8
  "*Indentation for `tableswitch' case lines.")

(defvar jasmin-unknown-line-indent 32
  "*Indentation for lines not recognized as valid.")

(defvar jasmin-lookupswitch-colon-column 16
  "*Column position of colon in a `lookupswitch' case line.")

(defvar jasmin-fix-whitespace t
  "*If non-nil, whitespace within a line can be adjusting when indenting.")

(defvar jasmin-case-default-face font-lock-keyword-face
  "*Font-lock face to use for `default' switch case.")

(defvar jasmin-class-face font-lock-function-name-face
  "*Font-lock face to use for class names and specs.")

(defvar jasmin-comment-face font-lock-comment-face
  "*Font-lock face to use for comments.")

(defvar jasmin-default-face 'default
  "*Font-lock face to use for default text.")

(defvar jasmin-directive-face font-lock-keyword-face
  "*Font-lock face to use for directive names.")

(defvar jasmin-field-face font-lock-variable-name-face
  "*Font-lock face to use for field specs.")

(defvar jasmin-instruction-face font-lock-keyword-face
  "*Font-lock face to use for instruction names.")

(defvar jasmin-keyword-face font-lock-keyword-face
  "*Font-lock face to use for keywords.")

(defvar jasmin-label-face font-lock-function-name-face
  "*Font-lock face to use for labels.")

(defvar jasmin-labelref-face font-lock-function-name-face
  "*Font-lock face to use for labels.")

(defvar jasmin-method-face font-lock-function-name-face
  "*Font-lock face to use for method names.")

(defvar jasmin-signature-face font-lock-function-name-face
  "*Font-lock face to use for signatures.")

(defvar jasmin-spec-face font-lock-keyword-face
  "*Font-lock face to use for access specifiers.")

(defvar jasmin-todo-face font-lock-function-name-face
  "*Font-lock face to use for \"to-do\" comments.")

(defvar jasmin-variable-face font-lock-variable-name-face
  "*Font-lock face to use for variable numbers and names.")

(defvar jasmin-mode-hook nil
  "*Hook run after `jasmin-mode' has finished all other mode initialization.")

;; Auto-Mode:

(if jasmin-edit-auto-mode-alist
    (let ((ext "\\.j\\'"))
      (or (assoc ext auto-mode-alist)
          (setq auto-mode-alist (append (list (cons ext 'jasmin-mode))
                                        auto-mode-alist)))))

;; Directive Table:

(defmacro jasmin-directive-type (directive)
  (list 'aref directive 0))

(defmacro jasmin-directive-syntax (directive)
  (list 'aref directive 1))

(defconst jasmin-directives
  '(
    (catch  . [method (class-name "from" label "to" label "using" label)])
    (class  . [global (access-spec class-name)])
    (end    . [global ("method")])
    (field  . [global
               (access-spec field-name &opt signature "=" value)])
    (implements . [global (class-name)])
    (limit  . [method (("locals" "stack") integer)])
    (line   . [method (integer)])
    (method . [global (access-spec method-name-and-signature)])
    (source . [global (file-name)])
    (super  . [global (class-name)])
    (throws . [method (class-name)])
    (var    . [method
               (var-number "=" var-name signature "from" label "to" label)])
    ))

(defconst jasmin-directive-completions
  (mapcar (function (lambda (n)
                      (cons (concat "." (prin1-to-string (car n))) (cdr n))))
          jasmin-directives))

;; Instruction Table:

(defmacro jasmin-instruction-syntax (instruction)
  (list 'aref instruction 0))

(defmacro jasmin-instruction-help (instruction)
  (list 'aref instruction 1))

(defconst jasmin-instructions
  '(
    (aaload . [() "Load object reference from array"])
    (aastore . [() "Store into object reference array"])
    (aconst_null . [() "Push null object"])
    (aload . [(var-number)
              "Load object reference from local variable"])
    (aload_0 . [() "Load object reference from local variable 0"])
    (aload_1 . [() "Load object reference from local variable 1"])
    (aload_2 . [() "Load object reference from local variable 2"])
    (aload_3 . [() "Load object reference from local variable 3"])
    (anewarray . [(class-name)
                  "Allocate a new array of references to objects"])
    (areturn . [() "Return object reference from function"])
    (arraylength . [() "Get length of array"])
    (astore . [(var-number)
               "Store object reference into local variable"])
    (astore_0 . [() "Store object reference into local variable 0"])
    (astore_1 . [() "Store object reference into local variable 1"])
    (astore_2 . [() "Store object reference into local variable 2"])
    (astore_3 . [() "Store object reference into local variable 3"])
    (athrow . [() "Throw exception or error"])
    (baload . [() "Load signed byte from array"])
    (bastore . [() "Store into signed byte array"])
    (bipush . [(integer) "Push one-byte signed integer"])
    (breakpoint . [() "Stop and pass control to breakpoint handler"])
    (caload . [() "Load character from array"])
    (castore . [() "Store into character array"])
    (checkcast . [(class-name) "Make sure object is of given type"])
    (d2f . [() "Double float to single float conversion"])
    (d2i . [() "Double float to integer conversion"])
    (d2l . [() "Double float to long integer conversion"])
    (dadd . [() "Double float add"])
    (daload . [() "Load double float from array"])
    (dastore . [() "Store into double float array"])
    (dcmpg . [() "Double float compare (1 on NaN)"])
    (dcmpl . [() "Double float compare (-1 on NaN)"])
    (dconst_0 . [() "Push double float 0"])
    (dconst_1 . [() "Push double float 1"])
    (ddiv . [() "Double float divide"])
    (dload . [(var-number)
              "Load double float from local variable"])
    (dload_0 . [() "Load double float from local variable 0"])
    (dload_1 . [() "Load double float from local variable 1"])
    (dload_2 . [() "Load double float from local variable 2"])
    (dload_3 . [() "Load double float from local variable 3"])
    (dmul . [() "Double float multiply"])
    (dneg . [() "Double float negate"])
    (drem . [() "Double float remainder"])
    (dreturn . [() "Return double float from function"])
    (dstore . [(var-number)
               "Store double float into local variable"])
    (dstore_0 . [() "Store double float into local variable 0"])
    (dstore_1 . [() "Store double float into local variable 1"])
    (dstore_2 . [() "Store double float into local variable 2"])
    (dstore_3 . [() "Store double float into local variable 3"])
    (dsub . [() "Double float subtract"])
    (dup . [() "Duplicate top stack word"])
    (dup2 . [() "Duplicate top two stack words"])
    (dup2_x1 . [() "Duplicate top two stack words and put two down"])
    (dup2_x2 . [() "Duplicate top two stack words and put three down"])
    (dup_x1 . [() "Duplicate top stack word and put two down"])
    (dup_x2 . [() "Duplicate top stack word and put three down"])
    (f2d . [() "Single float to double float conversion"])
    (f2i . [() "Single float to integer conversion"])
    (f2l . [() "Single float to long integer conversion"])
    (fadd . [() "Single float add"])
    (faload . [() "Load single float from array"])
    (fastore . [() "Store into single float array"])
    (fcmpg . [() "Single float compare (1 on NaN)"])
    (fcmpl . [() "Single float compare (-1 on NaN)"])
    (fconst_0 . [() "Push single float 0"])
    (fconst_1 . [() "Push single float 1"])
    (fconst_2 . [() "Push single float 2"])
    (fdiv . [() "Single float divide"])
    (fload . [(var-number)
              "Load single float from local variable"])
    (fload_0 . [() "Load single float from local variable 0"])
    (fload_1 . [() "Load single float from local variable 1"])
    (fload_2 . [() "Load single float from local variable 2"])
    (fload_3 . [() "Load single float from local variable 3"])
    (fmul . [() "Single float multiply"])
    (fneg . [() "Single float negate"])
    (frem . [() "Single float remainder"])
    (freturn . [() "Return single float from function"])
    (fstore . [(var-number)
               "Store single float into local variable"])
    (fstore_0 . [() "Store single float into local variable 0"])
    (fstore_1 . [() "Store single float into local variable 1"])
    (fstore_2 . [() "Store single float into local variable 2"])
    (fstore_3 . [() "Store single float into local variable 3"])
    (fsub . [() "Single float subtract"])
    (getfield . [(field-spec signature) "Fetch field from object"])
    (getstatic . [(field-spec signature) "Get static field from class"])
    (goto . [(label) "Branch always"])
    (goto_w . [(label) "Branch always (wide index)"])
    (i2d . [() "Integer to double float conversion"])
    (i2f . [() "Integer to single float conversion"])
    (i2l . [() "Integer to long integer conversion"])
    (iadd . [() "Integer add"])
    (iaload . [() "Load integer from array"])
    (iand . [() "Integer boolean AND"])
    (iastore . [() "Store into integer array"])
    (iconst_0 . [() "Push integer constant 0"])
    (iconst_1 . [() "Push integer constant 1"])
    (iconst_2 . [() "Push integer constant 2"])
    (iconst_3 . [() "Push integer constant 3"])
    (iconst_4 . [() "Push integer constant 4"])
    (iconst_5 . [() "Push integer constant 5"])
    (iconst_m1 . [() "Push integer constant -1"])
    (idiv . [() "Integer divide"])
    (if_acmpeq . [(label) "Branch if object references are equal"])
    (if_acmpne . [(label) "Branch if object references are not equal"])
    (if_icmpeq . [(label) "Branch if integers equal"])
    (if_icmpge . [(label) "Branch if integer greater than or equal to"])
    (if_icmpgt . [(label) "Branch if integer greater than"])
    (if_icmple . [(label) "Branch if integer less than or equal to"])
    (if_icmplt . [(label) "Branch if integer less than"])
    (if_icmpne . [(label) "Branch if integers not equal"])
    (ifeq . [(label) "Branch if equal to 0"])
    (ifge . [(label) "Branch if greater than or equal to 0"])
    (ifgt . [(label) "Branch if greater than 0"])
    (ifle . [(label) "Branch if less than or equal to 0"])
    (iflt . [(label) "Branch if less than 0"])
    (ifne . [(label) "Branch if not equal to 0"])
    (ifnonnull . [(label) "Branch if not null"])
    (ifnull . [(label) "Branch if null"])
    (iinc . [(var-number integer)
             "Increment local variable by constant"])
    (iload . [(var-number) "Load integer from local variable"])
    (iload_0 . [() "Load integer from local variable 0"])
    (iload_1 . [() "Load integer from local variable 1"])
    (iload_2 . [() "Load integer from local variable 2"])
    (iload_3 . [() "Load integer from local variable 3"])
    (imul . [() "Integer multiply"])
    (ineg . [() "Integer negate"])
    (instanceof . [(class-name) "Determine if object is of given type"])
    (int2byte . [() "Integer to signed byte conversion"])
    (int2char . [() "Integer to char conversion"])
    (int2short . [() "Integer to short conversion"])
    (invokenonvirtual . [(method-spec)
                         "Invoke instance method via compile-time type"])
    (invokestatic . [(method-spec) "Invoke class (static) method"])
    (invokevirtual . [(method-spec)
                      "Invoke instance method via run-time type"])
    (ior . [() "Integer boolean OR"])
    (irem . [() "Integer remainder"])
    (ireturn . [() "Return integer from function"])
    (ishl . [() "Integer shift left"])
    (ishr . [() "Integer shift right"])
    (istore . [(var-number) "Store integer into local variable"])
    (istore_0 . [() "Store integer into local variable 0"])
    (istore_1 . [() "Store integer into local variable 1"])
    (istore_2 . [() "Store integer into local variable 2"])
    (istore_3 . [() "Store integer into local variable 3"])
    (isub . [() "Integer subtract"])
    (iushr . [() "Integer logical shift right"])
    (ixor . [() "Integer boolean XOR"])
    (jsr . [(label) "Jump subroutine"])
    (jsr_w . [(label) "Jump subroutine (wide index)"])
    (l2d . [() "Long integer to double float conversion"])
    (l2f . [() "Long integer to float conversion"])
    (l2i . [() "Long integer to integer conversion"])
    (ladd . [() "Long integer add"])
    (laload . [() "Load long integer from array"])
    (land . [() "Long integer boolean AND"])
    (lastore . [() "Store into long integer array"])
    (lcmp . [() "Long integer compare"])
    (lconst_0 . [() "Push long integer constant 0"])
    (lconst_1 . [() "Push long integer constant 1"])
    (ldc . [(value) "Push item from constant pool"])
    (ldc_w . [(value) "Push item from constant pool (wide)"])
    (ldiv . [() "Long integer divide"])
    (lload . [(var-number)
              "Load long integer from local variable"])
    (lload_0 . [() "Load long integer from local variable 0"])
    (lload_1 . [() "Load long integer from local variable 1"])
    (lload_2 . [() "Load long integer from local variable 2"])
    (lload_3 . [() "Load long integer from local variable 3"])
    (lmul . [() "Long integer multiply"])
    (lneg . [() "Long integer negate"])
    (lookupswitch . [() "Access jump table by key match and jump"])
    (lor . [() "Long integer boolean OR"])
    (lrem . [() "Long integer remainder"])
    (lreturn . [() "Return long integer from function"])
    (lshl . [() "Long integer shift left"])
    (lshr . [() "Long integer arithmetic shift right"])
    (lstore . [(var-number)
               "Store long integer into local variable"])
    (lstore_0 . [() "Store long integer into local variable 0"])
    (lstore_1 . [() "Store long integer into local variable 1"])
    (lstore_2 . [() "Store long integer into local variable 2"])
    (lstore_3 . [() "Store long integer into local variable 3"])
    (lsub . [() "Long integer subtract"])
    (lushr . [() "Long integer logical shift right"])
    (lxor . [() "Long integer boolean XOR"])
    (monitorenter . [() "Enter monitored region of code"])
    (monitorexit . [() "Exit monitored region of code"])
    (multianewarray . [(array-signature integer)
                       "Allocate a new multi-dimensional array"])
    (new . [(class-name) "Create new object"])
    (newarray . [(array-type) "Allocate new array"])
    (nop . [() "Do nothing"])
    (pop . [() "Pop top stack word"])
    (pop2 . [() "Pop top two stack words"])
    (putfield . [(field-spec signature) "Set field in object"])
    (putstatic . [(field-spec signature) "Set static field in class"])
    (ret . [(var-number) "Return from subroutine"])
    (return . [() "Return (void) from procedure"])
    (saload . [() "Load short from array"])
    (sastore . [() "Store into short array"])
    (sipush . [(integer) "Push two-byte signed integer"])
    (swap . [() "Swap top two stack words"])
    (tableswitch . [(integer) "Access jump table by index and jump"])
    ))

(defconst jasmin-instruction-completions
  (mapcar (function (lambda (n) (cons (prin1-to-string (car n)) (cdr n))))
          jasmin-instructions))

;; Syntax Misc.:

(defconst jasmin-whitespace-chars-string
  " \t\f")
(defconst jasmin-whitespace-char-regexp
  (concat "[" jasmin-whitespace-chars-string "]"))
(defconst jasmin-whitespace-regexp
  (concat jasmin-whitespace-char-regexp "+"))
(defconst jasmin-optional-whitespace-regexp
  (concat jasmin-whitespace-char-regexp "*"))

(defconst jasmin-label-name-regexp
  (let ((no-nos (concat "-" jasmin-whitespace-chars-string "\n\^m=:\\.\"")))
    (concat "[^" no-nos "0-9][^" no-nos "]*")))

(defconst jasmin-access-specs
  '(abstract final interface native private protected public static
             synchronized transient volatile))

;; Character Syntax Table:

(defvar jasmin-mode-syntax-table nil)

(if (not jasmin-mode-syntax-table)
    (progn
      (setq jasmin-mode-syntax-table (make-syntax-table))
      (mapcar (function (lambda (n)
                          (modify-syntax-entry (aref n 0)
                                               (aref n 1)
                                               jasmin-mode-syntax-table)))
              '(
                ;; whitespace (` ')
                [?\^m " "]
                [?\f  " "]
                [?\n  " "]
                [?\t  " "]
                ;; word constituents (`w')
                [?<  "w"]
                [?>  "w"]
                [?\; "w"]
                [?\[ "w"]
                [?_  "w"]
                ;; symbol constituents (`_')
                ;; punctuation (`.')
                ;; open paren (`(')
                [?\( "("]
                ;; close paren (`)')
                [?\) ")"]
                ;; string quote ('"')
                [?\" "\""]
                ))))

;; Keymap:

(defvar jasmin-mode-map nil)

(if (not jasmin-mode-map)
    (progn
      (setq jasmin-mode-map (make-sparse-keymap))
      (mapcar (function (lambda (n)
                          (define-key jasmin-mode-map (aref n 0) (aref n 1))))
              '(
                ["\C-c\C-b" jasmin-submit-bug-report]
                ["\C-c\C-h" jasmin-help]
                ["\C-j"     jasmin-electric-lfd]
                ["\C-m"     jasmin-electric-ret]
                [";"        jasmin-electric-semi]
                ))))

;; Abbrev Table:

;;(defvar jasmin-mode-abbrev-table nil
;;  "")

;;(if (not jasmin-mode-abbrev-table)
;;    (progn
;;      (define-abbrev-table 'jasmin-mode-abbrev-table ())
;;      ;; TODO: Define abbrev table here.
;;      ))

;; Font Lock:

(defun jasmin-font-lock-for-syntax (directive-p name-symbol syntax)
  (let* ((regexp-front (concat "^"
                               jasmin-optional-whitespace-regexp
                               "\\("
                               (if directive-p "\\." "")
                               (regexp-quote (prin1-to-string name-symbol))
                               "\\>\\)"))
         (regexp-back "")
         (arg-count 1)
         (arg-list (list (list arg-count
                               (if directive-p
                                   'jasmin-directive-face
                                 'jasmin-instruction-face)))))
    (mapcar
     (function
      (lambda (token)
        (let* ((data (jasmin-regexp-data-for-token token))
               token-regexp token-face token-extra)
          (if (eq data 'opt)
              ()
            (setq token-regexp (aref data 0)
                  token-face   (aref data 1)
                  token-extra  (aref data 2)
                  regexp-front (concat regexp-front
                                       "\\("
                                       jasmin-optional-whitespace-regexp
                                       token-regexp)
                  regexp-back  (concat regexp-back "\\)?")
                  arg-count    (1+ arg-count)
                  arg-list     (append arg-list
                                       (list (list arg-count token-face t t))))
            (if token-extra
                (setq arg-count (1+ arg-count)
                      arg-list  (append
                                 arg-list
                                 (list (list arg-count token-face t t)))))))))
     syntax)
    (append (list (concat regexp-front regexp-back))
            arg-list)))

(defconst jasmin-other-font-lock-keywords
  (let* ((line-beg (concat "^" jasmin-optional-whitespace-regexp)))
    (list
     ;; Comment: todo
     (list (concat "\\(" line-beg "\\|" jasmin-whitespace-char-regexp "\\)"
                   "\\(;+" jasmin-optional-whitespace-regexp "\\)"
                   "\\(!!!\\|[tT][oO][dD][oO]:\\)\\([^\n]*\\)")
           (list 2 'jasmin-comment-face)
           (list 3 'jasmin-todo-face)
           (list 4 'jasmin-comment-face))
     ;; Comment: normal
     (list (concat "\\(^" jasmin-optional-whitespace-regexp "\\|"
                   jasmin-whitespace-char-regexp "\\)\\(;[^\n]*\\)")
           (list 2 'jasmin-comment-face))
     ;; Switch default case (check before label).
     (list (concat line-beg "\\(default\\)" jasmin-optional-whitespace-regexp
                   ":")
           (list 1 'jasmin-case-default-face))
     ;; Label.
     (list (concat line-beg "\\(" jasmin-label-name-regexp "\\)"
                   jasmin-optional-whitespace-regexp ":")
           (list 1 'jasmin-label-face)))))

(defun jasmin-font-lock-keywords ()
  (let (data)
    (setq data (append jasmin-other-font-lock-keywords
                       (mapcar (function
                                (lambda (n)
                                  (jasmin-font-lock-for-syntax
                                   t
                                   (car n)
                                   (jasmin-directive-syntax (cdr n)))))
                               jasmin-directives)))
    (if jasmin-fontify-instructions
        (setq data
              (append data
                      (mapcar (function
                               (lambda (n)
                                 (jasmin-font-lock-for-syntax
                                  nil
                                  (car n)
                                  (jasmin-instruction-syntax (cdr n)))))
                              (reverse jasmin-instructions)))))
    data))

(defun jasmin-regexp-data-for-token (token)
  ;; TODO: `default-regexp' is a temporary measure. Make more precise ones.
  (if (eq token '&opt)
      'opt
    (let* ((default-regexp "[^ \t\f\n\^m]+\\>")
           (default-data (vector default-regexp 'jasmin-default-face nil)))
      (cond ((eq token 'access-spec)
             (vector
              (concat "\\("
                      (mapconcat (function
                                  (lambda (n)
                                    (concat jasmin-optional-whitespace-regexp
                                            (prin1-to-string n))))
                                 jasmin-access-specs
                                 "\\|")
                      "\\)*\\>")
              'jasmin-spec-face
              t))
            ((eq token 'array-signature) default-data)
            ((eq token 'array-type) default-data)
            ((eq token 'class-name)
             (vector default-regexp 'jasmin-class-face nil))
            ((eq token 'field-name) default-data)
            ((eq token 'field-spec)
             (vector default-regexp 'jasmin-field-face nil))
            ((eq token 'file-name) default-data)
            ((eq token 'integer)
             (vector "[+-]?[0-9]+\\>" 'jasmin-default-face nil))
            ((eq token 'label)
             (vector jasmin-label-name-regexp 'jasmin-label-face nil))
            ((eq token 'method-name-and-signature)
             (vector default-regexp 'jasmin-method-face nil))
            ((eq token 'method-spec)
             (vector default-regexp 'jasmin-method-face nil))
            ((eq token 'signature)
             (vector default-regexp 'jasmin-signature-face nil))
            ((eq token 'value)
             (vector "[^\n\^m]+\\>" 'jasmin-default-face nil))
            ((eq token 'var-name)
             (vector default-regexp 'jasmin-variable-face nil))
            ((eq token 'var-number)
             (vector "[0-9]+\\>" 'jasmin-variable-face nil))
            ((stringp token)
             (vector (if (string= token "=")
                         token
                       (concat (regexp-quote token) "\\>"))
                     'jasmin-keyword-face
                     nil))
            ((listp token)
             (vector (concat "\\("
                             (mapconcat '(lambda (n)
                                           (regexp-quote n))
                                        token
                                        "\\|")
                             "\\)\\>")
                     'jasmin-keyword-face
                     t))
            (t (error "Internal Error: Unknown token \"%s\"" token))))))

(defconst jasmin-font-lock-keywords
  (jasmin-font-lock-keywords))

;; Indenting:

(defun jasmin-force-no-space (space-begin space-end)
  (or (= space-begin space-end)
      (delete-region space-begin space-end)))

(defun jasmin-force-single-space (space-begin space-end)
  (if (/= (- space-end space-begin) 1)
      (save-excursion
        (goto-char space-begin)
        (delete-region space-begin space-end)
        (insert " "))))

(defun jasmin-in-method-p (context)
  (memq context '(method lookupswitch tableswitch)))

(defun jasmin-indent-line () ;;&optional arg)
  ;;(interactive "P")
  (let ((context (jasmin-line-context)))
    (beginning-of-line)
    (delete-region (point) (progn (skip-chars-forward " \t\f")
                                  (point)))
    (cond
     ;; Blank line.
     ((looking-at "\\([ \t\f]*\\)$")
      (indent-to-column (if (jasmin-in-method-p context)
                            jasmin-instruction-indent
                          0)))
     ;; Comment.
     ((looking-at ";")
      (indent-to-column (if (jasmin-in-method-p context)
                            jasmin-instruction-indent
                          0)))
     ;; Directive.
     ((looking-at "\\.\\([a-zA-Z]+\\)")
      (indent-to-column
       (let* ((cell (assq (intern (jasmin-buffer-match-string 1))
                          jasmin-directives))
              (type (if cell (jasmin-directive-type (cdr cell)) nil)))
         (cond ((eq type 'method) jasmin-method-directive-indent)
               ((eq type 'global) jasmin-global-directive-indent)
               (t jasmin-unknown-line-indent)))))
     ;; Lookupswitch case.
     ((and (eq context 'lookupswitch)
           (looking-at "\\([0-9]+\\|default\\)\\([ \t\f]*\\):\\([ \t\f]*\\)"))
      (if jasmin-fix-whitespace
          (progn
            (jasmin-force-single-space (match-beginning 3) (match-end 3))
            (jasmin-force-single-space (match-beginning 2) (match-end 2))))
      (indent-to-column (max (- jasmin-lookupswitch-colon-column
                                (- (match-end 1) (match-beginning 1))
                                (if jasmin-fix-whitespace
                                    1
                                  (- (match-end 2) (match-beginning 2))))
                             jasmin-instruction-indent)))
     ;; Tableswitch `default:' case or erroneous one.
     ;; Note: Check must follow lookupswitch and precede tableswitch,label.
     ((looking-at "\\(default\\)\\([ \t\f]*\\):\\([ \t\f]*\\)")
      (if jasmin-fix-whitespace
          (progn
            (jasmin-force-single-space (match-beginning 3) (match-end 3))
            (jasmin-force-no-space (match-beginning 2) (match-end 2))))
      (indent-to-column (if (eq context 'tableswitch)
                            jasmin-tableswitch-case-indent
                          jasmin-unknown-line-indent)))
     ;; Tableswitch case.
     ((eq context 'tableswitch)
      (indent-to-column jasmin-tableswitch-case-indent))
     ;; Label (must be checked before instruction).
     ((looking-at (concat jasmin-label-name-regexp "\\([ \t\f]*\\):"))
      (if jasmin-fix-whitespace
          (jasmin-force-no-space (match-beginning 1) (match-end 1)))
      (indent-to-column jasmin-label-indent))
     ;; Instruction.
     ((and (looking-at "[a-zA-Z][a-zA-Z0-9_]*\\>")
           (eq context 'method))
      (indent-to-column jasmin-instruction-indent))
     ;; Other.
     (t
      (indent-to-column jasmin-unknown-line-indent)))))

;; TODO: defun jasmin-indent-region-function (start end to-column)

(defun jasmin-line-context ()
  (save-excursion
    (let (context)
      (while (not context)
        (setq context
              (if (not (zerop (forward-line -1)))
                  'global
                (back-to-indentation)
                (cond ((looking-at "\\.method\\>")     'method)
                      ((looking-at "\\.end\\>")        'global)
                      ((looking-at "tableswitch\\>")   'tableswitch)
                      ((looking-at "lookupswitch\\>")  'lookupswitch)
                      ((looking-at "default[ \t\f]*:") 'method)
                      (t nil)))))
      context)))

;; Mode Function:

(defun jasmin-mode ()
  "Major mode for editing Jasmin Java bytecode assembler files.

Current major features are:

  * Indenting, with a little non-indenting formatting thrown in.

  * Font-lock fontifying.  (If you find this to be too slow, set
    `jasmin-fontify-instructions' to nil.)

  * Quick-reference help on instructions and directives via `\\[jasmin-help]'.

See `jasmin.el' for the option variables that can be set.


Key bindings:

\\{jasmin-mode-map}

The Web page is: http://www.neilvandyke.org/jasmin-emacs/"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'indent-line-function)
  ;;(make-local-variable 'indent-region-function)
  (make-local-variable 'indent-tabs-mode)
  (setq font-lock-defaults     '(jasmin-font-lock-keywords t)
        indent-line-function   'jasmin-indent-line
        ;;indent-region-function 'jasmin-indent-region
        indent-tabs-mode       (not jasmin-i-hate-tabs)
        ;;local-abbrev-table     jasmin-mode-abbrev-table
        major-mode             'jasmin-mode
        mode-name              "Jasmin")
  (use-local-map jasmin-mode-map)
  (set-syntax-table jasmin-mode-syntax-table)
  (if jasmin-turn-on-font-lock
      (turn-on-font-lock))
  (run-hooks 'jasmin-mode-hook))

;; Electric Keys:

(defun jasmin-electric-lfd (arg)
  (interactive "P")
  (reindent-then-newline-and-indent))

(defun jasmin-electric-ret (arg)
  (interactive "P")
  (reindent-then-newline-and-indent))

(defun jasmin-electric-semi (arg)
  (interactive "P")
  ;; TODO: Handle case of adding comment after other stuff on a line.
  (insert ";"))

;; Help:

(defconst jasmin-help-buffer-name "*Jasmin Help*")

(defun jasmin-help ()
  (interactive)
  (let* ((completions (append jasmin-directive-completions
                              jasmin-instruction-completions))
         (possible (save-excursion
                     (back-to-indentation)
                     (if (looking-at "\\.?[a-z0-9_]+")
                         (jasmin-buffer-match-string 0))))
         (input (completing-read "Jasmin help on: "
                                 completions
                                 nil
                                 t
                                 (if (assoc possible completions)
                                     possible))))
    (if (and input (not (string= "" input)))
        (let* ((name   input)
               (directive-p (string-match "^\\.\\(.*\\)$" input))
               (data (if directive-p
                         (cdr (assq (intern (match-string 1 input))
                                    jasmin-directives))
                       (cdr (assq (intern input) jasmin-instructions)))))
          (save-excursion
            (jasmin-prepare-help-buffer)
            (jasmin-insert-help-label "Syntax:       ")
            (jasmin-insert-help-syntax name
                                       (if directive-p
                                           (jasmin-directive-syntax data)
                                         (jasmin-instruction-syntax data)))
            (insert "\n")
            (if (not directive-p)
                (progn
                  (jasmin-insert-help-label "Description:  ")
                  (insert (jasmin-instruction-help data) "\n"))))
          (jasmin-finish-help-buffer)))))

(defun jasmin-insert-help-syntax (name syntax)
  (jasmin-insert-help-keyword name)
  (let (has-optional)
    (mapcar (function
             (lambda (token)
               (cond
                ((eq token '&opt)
                 (setq has-optional t)
                 (insert " ["))
                ((stringp token)
                 (insert " ")
                 (jasmin-insert-help-keyword token))
                ((symbolp token)
                 (insert " ")
                 (jasmin-insert-help-arg (prin1-to-string token)))
                ((listp token)
                 (insert " (")
                 (let (preceded-p)
                   (mapcar (function
                            (lambda (n)
                              (if preceded-p
                                  (insert " |")
                                (setq preceded-p t))
                              (insert " ")
                              (jasmin-insert-help-keyword n)))
                           token))
                 (insert " )")))))
            syntax)
    (if has-optional
        (insert " ]"))))

(defun jasmin-insert-help-label (str)
  (jasmin-insert-with-face str 'bold-italic))

(defun jasmin-insert-help-keyword (str)
  (jasmin-insert-with-face str 'bold))

(defun jasmin-insert-help-arg (str)
  (if window-system
      (jasmin-insert-with-face str 'italic)
    (insert "<" str ">")))

(defun jasmin-prepare-help-buffer ()
  (set-buffer (get-buffer-create jasmin-help-buffer-name))
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max)))

(defun jasmin-finish-help-buffer ()
  (let* ((buffer (get-buffer jasmin-help-buffer-name))
         (window (get-buffer-window buffer)))
    (set-buffer buffer)
    (goto-char (point-min))
    (setq buffer-read-only t)
    (if (not window)
        (let ((orig-window-height (window-height)))
          (if (< orig-window-height (* window-min-height 2))
              (progn
                (message "Not enough space to split window!")
                (set-window-buffer (selected-window) buffer))
            (set-window-buffer
             (split-window nil
                           (max (- orig-window-height
                                   (max jasmin-help-window-size
                                        window-min-height))
                                window-min-height))
             buffer))))))

;; Header and Footer:

(defun jasmin-add-file-header ()
  (interactive)
  (goto-char (point-min))
  (insert ";; $" "Id" "$\n\n"
          (if buffer-file-name
              (concat ".source " (file-name-nondirectory buffer-file-name))
            ";;.source ")
          "\n.class  public ")
  (save-excursion
    (insert "\n.super  java/lang/Object\n\n")))

(defun jasmin-add-file-footer ()
  (interactive)
  (goto-char (point-max))
  (save-excursion
    (insert "\n\n;;EOF")))

;; Bug Reporting:

;; (defun jasmin-submit-bug-report ()
;;   "Submit via mail a bug report on `jasmin'."
;;   (interactive)
;;   (require 'reporter)
;;   (reporter-submit-bug-report
;;    jasmin-maintainer-address
;;    (concat "jasmin.el " jasmin-version " " jasmin-vc-id)
;;    '(jasmin-case-default-face
;;      jasmin-class-face
;;      jasmin-comment-face
;;      jasmin-default-face
;;      jasmin-directive-face
;;      jasmin-edit-auto-mode-alist
;;      jasmin-field-face
;;      jasmin-fix-whitespace
;;      jasmin-fontify-instructions
;;      jasmin-global-directive-indent
;;      jasmin-help-window-size
;;      jasmin-i-hate-tabs
;;      jasmin-instruction-face
;;      jasmin-instruction-indent
;;      jasmin-keyword-face
;;      jasmin-label-face
;;      jasmin-label-indent
;;      jasmin-labelref-face
;;      jasmin-lookupswitch-colon-column
;;      jasmin-method-directive-indent
;;      jasmin-method-face
;;      jasmin-mode-hook
;;      jasmin-signature-face
;;      jasmin-spec-face
;;      jasmin-tableswitch-case-indent
;;      jasmin-todo-face
;;      jasmin-turn-on-font-lock
;;      jasmin-unknown-line-indent
;;      jasmin-variable-face)))

;; Utility:

(defun jasmin-buffer-match-string (n)
  (if (fboundp 'buffer-substring-no-properties)
      (buffer-substring-no-properties (match-beginning n) (match-end n))
    (buffer-substring (match-beginning n) (match-end n))))

(defun jasmin-insert-with-face (str face)
  (jasmin-set-face-region (prog1 (point)
                            (insert str))
                          (point)
                          face))

(defun jasmin-set-face-region (begin end face)
  (put-text-property begin end 'face face))

;; End:

(provide 'jasmin)

;; jasmin.el ends here
