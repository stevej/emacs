;; set sane exec path before anything else.
(push "/usr/local/bin" exec-path)

(require 'cl)
(defvar *emacs-load-start* (current-time))


;; FIXME: make this work across OS's
(add-to-list 'load-path "/Users/stevej/.opam/4.01.0/share/emacs/site-lisp/")

;; DO NOT ADD .emacs.d to your load-path
(add-to-list 'load-path "~/.emacs.d/customizations")
(add-to-list 'load-path "~/.emacs.d/utilities")
(add-to-list 'load-path "~/.emacs.d/utilities/ert")
(add-to-list 'load-path "~/.emacs.d/utilities/jump")
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/themes")

(load "~/.emacs.d/utilities/load-directory.el")
(mapcar 'load-directory '("~/.emacs.d/utilities"))

(vendor 'auto-complete)
(vendor 'anything)
(vendor 'coffee-mode)
(vendor 'color-theme)
(vendor 'coq)
(vendor 'deft)
(vendor 'emacsd-tile)
(vendor 'forth-mode)
(vendor 'fsharp)
(vendor 'full-ack)
(vendor 'glsl-mode)
(vendor 'go-mode)
(vendor 'google-c-style)
(vendor 'guess-style)
(vendor 'haskell-mode)
(vendor 'idris-mode)
(vendor 'io-mode)
(vendor 'jasmin)
(vendor 'js2-mode)
(vendor 'lua-mode)
(vendor 'markdown-mode)
(vendor 'multi-web-mode)
(vendor 'mustache-mode)
(vendor 'nasm-mode)
(vendor 'nav)
(vendor 'nxml-mode)
(vendor 'peg-mode)
(vendor 'piglatin-mode)
(vendor 'powerline)
(vendor 'powerline-themes)
(vendor 'powerline-separators)
(vendor 'quack)
(vendor 'racket-mode)
(vendor 'ruby-mode)
;(vendor 'rust-mode)
(vendor 'scala-mode2)
(vendor 'scion)
(vendor 'smart-tab)
(vendor 'smex)
(vendor 'sml-mode)
(vendor 'textile-mode)
(vendor 'textmate)
(vendor 'thrift-mode)
(vendor 'toml-mode)
(vendor 'two-mode-mode)
(vendor 'tuareg)
(vendor 'verilog-mode)
(vendor 'yaml-mode)



(setq package-archives
      '(("ELPA"           . "http://tromey.com/elpa/")
        ("SC"             . "http://joseito.republika.pl/sunrise-commander/")
        ("gnu"            . "http://elpa.gnu.org/packages/")
        ("marmalade"      . "http://marmalade-repo.org/packages/")
        ("melpa"          . "http://melpa.org/packages/")
        ("melpa-stable"   . "http://stable.melpa.org/packages/")
        ("org"            . "http://orgmode.org/elpa/"))
      package-pinned-packages
      '((arduino-mode        . "melpa")
        (company             . "melpa")
        (company-cabal       . "melpa")
        (company-ghc         . "melpa")
        (deft                . "melpa")
        (flycheck-rust       . "melpa")
        (gist                . "melpa")
        (gnuplot             . "melpa")
        (graphviz-dot-mode   . "melpa")
        (hamlet-mode         . "melpa")
        (haskell-emacs       . "melpa")
        (haskell-mode        . "melpa")
        (hindent             . "melpa")
        (ido-ubiquitous      . "melpa")
        (idris-mode          . "melpa")
;        (js2-mode            . "melpa")
;        (markdown-mode       . "melpa")
;        (paredit             . "melpa")
        (rainbow-delimiters  . "melpa")
        (rainbow-identifiers . "melpa")
;        (rust-mode           . "melpa")
        (smex                . "melpa")
;        (textile-mode        . "melpa")
;        (yaml-mode           . "melpa")
      el-get-user-packages '(ghc-mod)))


(package-initialize t)



;; This must be loaded last due to dependencies
(mapcar 'load-directory '("~/.emacs.d/customizations"))

(cond
 ((eq window-system 'ns) ; macosx
  ;; Invoke login shells, so that .profile or .bash_profile is read
  (setq shell-command-switch "-lc")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(js2-basic-offset 2)
 '(quack-programs
   (quote
    ("racket -I scheme" "/Users/stevej/bin/mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "klisp" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "mzscheme -il r6rs" "mzscheme -il typed-scheme" "mzscheme3m" "mzschemecgc" "racket" "racket -I r5rs" "racket -I r6rs" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(quack-remap-find-file-bindings-p nil)
 '(safe-local-variable-values
   (quote
    ((encoding . utf-8)
     (buffer-file-coding-system . utf-8-unix)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby"))))

 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(show-paren-mode t)
 '(coq-prog-args '("-I" "/Users/stevej/local/src/cpdt/src"))
 '(show-paren-mode t)
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "#559944"))))
 '(diff-context ((t nil)))
 '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
 '(diff-function ((t (:foreground "#00bbdd"))))
 '(diff-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
 '(diff-hunk-header ((t (:foreground "#fbde2d"))))
 '(diff-nonexistent ((t (:inherit diff-file-header :strike-through nil))))
 '(diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background "#182042"))))
 '(diff-removed ((t (:foreground "#de1923")))))
