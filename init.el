;; set sane exec path before anything else.
(push "/opt/local/bin" exec-path)
(push "/usr/local/bin" exec-path)

(require 'cl)
(defvar *emacs-load-start* (current-time))

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/customizations")
(add-to-list 'load-path "~/.emacs.d/utilities")
(add-to-list 'load-path "~/.emacs.d/utilities/ert")
(add-to-list 'load-path "~/.emacs.d/utilities/jump")
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/themes")

(load "~/.emacs.d/load-directory.el")
(mapcar 'load-directory '("~/.emacs.d/utilities"))

(vendor 'anything)
(vendor 'clojure-mode)
(vendor 'color-theme)
(vendor 'configgy-mode)
(vendor 'coq)
(vendor 'csharp-mode)
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
(vendor 'http-twiddle)
(vendor 'incanter)
(vendor 'io-mode)
(vendor 'jasmin)
(vendor 'js2-mode)
(vendor 'lua-mode)
(vendor 'markdown-mode)
(vendor 'multi-web-mode)
(vendor 'magit)
(vendor 'nav)
(vendor 'nxml-mode)
(vendor 'piglatin-mode)
(vendor 'quack)
(vendor 'rinari)
(vendor 'rvm)
(vendor 'ruby-mode)
(vendor 'rust-mode)
(vendor 'scala-mode)
(vendor 'scion)
(vendor 'smart-tab)
(vendor 'smex)
(vendor 'sml-mode)
;;(vendor 'tea)
(vendor 'textile-mode)
(vendor 'textmate)
(vendor 'thrift-mode)
;(vendor 'two-mode-mode)
(vendor 'tuareg)
(vendor 'verilog-mode)
(vendor 'yaml-mode)

;; This must be loaded last due to dependencies
(mapcar 'load-directory '("~/.emacs.d/customizations"))

(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
                             (- (+ hi lo) (+ (first *emacs-load-start*) (second
                             *emacs-load-start*)))))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(ensime-sbt-program-name "/Users/stevej/bin/sbt")
 '(js2-basic-offset 2)
 '(quack-programs (quote ("klisp" "/Users/stevej/bin/mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "mzscheme -il r6rs" "mzscheme -il typed-scheme" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(quack-remap-find-file-bindings-p nil)
 '(safe-local-variable-values (quote ((buffer-file-coding-system . utf-8-unix) (ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby"))))
 '(show-paren-mode t))
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
