;;; rust-mode.el --- CC Mode derived mode for rust.

;; Author:     Graydon Hoare
;; Maintainer: Graydon Hoare
;; Created:    April 2009
;; Version:    See cc-mode.el
;; Keywords:   c languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a derived CC mode for the rust language, partly a
;; copy-and-paste job from
;; http://cc-mode.sourceforge.net/derived-mode-ex.el
;;
;; With further modifications by guesswork and looking through the
;; cc-langs.el file in the CC-mode distribution to get an idea for
;; the keyword/symbol classes.

;; Note: The interface used in this file requires CC Mode 5.30 or
;; later.

;;; Code:

(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  ;; Add rust, fallback to C.
  (c-add-language 'rust-mode 'c-mode))

;; rust type keywords
(c-lang-defconst c-primitive-type-kwds
  rust '("rec" "tup" "vec" "tag"
         "char" "str"
         "chan" "port"
         "task" "mod" "obj"
         "fn" "iter"
         "any"
         "bool" "int" "uint" "float" "big"
         "i8" "i16" "i32" "i64"
         "u8" "u16" "u32" "u64"))

;; rust has no prefixes for primitives
(c-lang-defconst c-primitive-type-prefix-kwds
  rust nil)

;; rust has no bitfields
(c-lang-defconst c-bitfield-kwds
  rust nil)

(c-lang-defconst c-type-modifier-kwds
  rust '("abs"
         "state" "gc"
         "impure" "unsafe"
         "const"))

;; declarator-block openings
(c-lang-defconst c-other-block-decl-kwds
  rust '("mod" "fn" "iter" "tag"))

;; type-defining declarators
(c-lang-defconst c-typedef-decl-kwds
  rust '("type"))

;; declaration modifiers
(c-lang-defconst c-modifier-kwds
  rust '("native" "mutable" "@" "&"))

;; declarators that refer to other namespaces
(c-lang-defconst c-ref-list-kwds
  rust '("use" "auth" "import" "export"))

;; ops that are used to form identifiers
(c-lang-defconst c-identifier-ops
  rust '((letf-assoc ".")
         (postfix "[" "]")))

;; additional declarator keywords
(c-lang-defconst c-other-decl-kwds
  rust '("let" "auto"))

;; statement keywords you can stick substatements directly after
(c-lang-defconst c-block-stmt-1-kwds
  rust '("self"))

;; statement keywords you can stick bracketed substatements after
(c-lang-defconst c-block-stmt-2-kwds
  rust '("for" "each" "if" "else" "alt" "while" "do" "case"))

;; statement keywords followed by a simple expression
(c-lang-defconst c-simple-stmt-kwds
  rust '("ret" "be" "put"
         "check" "prove" "claim"
         "log" "in"
         "yield" "join"
         "break" "cont"
         "fail" "drop"))

;; don't do 'case' the way it is in C
(c-lang-defconst c-case-kwds
  rust nil)

;; don't do 'case' or 'default' the way it is in C
(c-lang-defconst c-label-kwds
  rust nil)

;; don't highlight "struct", "union", "enum" as in C
(c-lang-defconst c-type-prefix-kwds
  rust nil)

;; don't highlight "struct", "union", "enum" as in C
(c-lang-defconst c-type-list-kwds
  rust nil)

;; don't highlight "struct", "union", "enum" as in C
(c-lang-defconst c-class-decl-kwds
  rust '("obj"))

;; don't highlight "extern"
(c-lang-defconst c-nonsymbol-sexp-kwds
 rust nil)

;; constants
(c-lang-defconst c-constant-kwds
  rust '("true" "false"))

;; operators
(c-lang-defconst c-operators
  rust '((prefix "#")
         (right-assoc ".")
         (postfix "[" "]" "(" ")")
         (prefix "!" "-")
         (prefix "++" "--")
         (postfix "++" "--")
         (left-assoc "*" "/" "%")
         (left-assoc "+" "-")
         (left-assoc "<<" ">>" ">>>")
         (left-assoc "<" "<=" ">=" ">")
         (left-assoc "==" "!=")
         (left-assoc "&" "~" "^" "|")
         (left-assoc "||" "&&")
         (left-assoc ",")
         (left-assoc "=" "*=" "/=" "+=" "-=" "<<=" ">>=" ">>>=" "&=" "|=" "^=")
         (lett-assoc "<|" "<+" "<-")
         (prefix "spawn" "thread" "bind" "@")
         (left-assoc "with" "as")))

;; keywords followed by non-type expressions in parens
(c-lang-defconst c-paren-nontype-kwds
  rust '("rec" "tup" "vec" "port" "chan" "meta"))

;; punctuation or paren syntax classes that have syntactic meaning
(c-lang-defconst c-other-op-syntax-tokens
  rust '("#" "{" "}" "(" ")" "[" "]" ";" ":" "," "=" "->" "//"))

(c-lang-defconst c-stmt-delim-chars-with-comma
  rust ";{}")

;; No c preprocessor in rust. The correct way to disable cpp-related
;; stuff in cc-mode appears to be to set c-opt-cpp-prefix to
;; nil. Unfortunately this causes font-lock-mode to not work
;; correctly (highlighting is performed when the buffer is loaded, but
;; subsequent typing isn't highlighted correctly).  As a hack, I'm
;; defining the cpp prefix to be an impossible regex, in this case a
;; word boundary in the middle of a word.
(c-lang-defconst c-opt-cpp-prefix rust "a\bc")

;; Make comment-region, probably other comment-related stuff use line
;; comments instead of block comments
(c-lang-defconst comment-start
  rust (c-lang-const c-line-comment-starter))

(defcustom rust-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in
rust mode.Each list item should be a regexp matching a single identifier.")

(defconst rust-font-lock-keywords-1 (c-lang-const c-matchers-1 rust)
  "Minimal highlighting for rust mode.")

(defconst rust-font-lock-keywords-2 (c-lang-const c-matchers-2 rust)
  "Fast normal highlighting for rust mode.")

(defconst rust-font-lock-keywords-3 (c-lang-const c-matchers-3 rust)
  "Accurate normal highlighting for rust mode.")

(defvar rust-font-lock-keywords rust-font-lock-keywords-3
  "Default expressions to highlight in rust mode.")

(defvar rust-mode-syntax-table nil
  "Syntax table used in rust-mode buffers.")
(or rust-mode-syntax-table
    (setq rust-mode-syntax-table
      (funcall (c-lang-const c-make-mode-syntax-table rust))))

(defvar rust-mode-abbrev-table nil
  "Abbreviation table used in rust-mode buffers.")

(c-define-abbrev-table 'rust-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  (list))

(defvar rust-mode-map (let ((map (c-make-inherited-keymap)))
              ;; Add bindings which are only useful for rust
              map)
  "Keymap used in rust-mode buffers.")

(easy-menu-define rust-menu rust-mode-map "Rust Mode Commands"
          ;; Can use `rust' as the language for `c-mode-menu'
          ;; since its definition covers any language.  In
          ;; this case the language is used to adapt to the
          ;; nonexistence of a cpp pass and thus removing some
          ;; irrelevant menu alternatives.
          (cons "rust" (c-lang-const c-mode-menu rust)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.rc$" . rust-crate-mode))

;;;###autoload
(defun rust-mode ()
  "Major mode for editing rust code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `rust-mode-hook'.

Key bindings:
\\{rust-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table rust-mode-syntax-table)
  (setq major-mode 'rust-mode
    mode-name "rust"
    local-abbrev-table rust-mode-abbrev-table
    abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars rust-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'rust-mode)
  (easy-menu-add rust-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'rust-mode-hook)
  (c-update-modeline))

(defun rust-crate-mode ()
  "Major mode for editing rust crate files.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `rust-mode-hook'.

Key bindings:
\\{rust-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table rust-mode-syntax-table)
  (setq major-mode 'rust-mode
    mode-name "rust"
    local-abbrev-table rust-mode-abbrev-table
    abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars rust-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'rust-mode)
  (easy-menu-add rust-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'rust-mode-hook)
  (c-update-modeline))

(provide 'rust-mode)

;;; rust-mode.el ends here
