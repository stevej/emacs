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

(load "~/.emacs.d/load-directory.el")
(mapcar 'load-directory '("~/.emacs.d/utilities"))

(vendor 'haskell-mode)
(vendor 'textmate)
(vendor 'configgy-mode)
(vendor 'magit)
(vendor 'clojure-mode)
(vendor 'incanter)
(vendor 'scala-mode)
(vendor 'thrift-mode)
(vendor 'sml-mode)
(vendor 'ruby-mode)
(vendor 'rinari)
(vendor 'nxml-mode)
(vendor 'textile-mode)
(vendor 'full-ack)
(vendor 'nav)
(vendor 'js2-mode)
(vendor 'yaml-mode)
(vendor 'smart-tab)
;;(vendor 'tea)
(vendor 'tuareg)
(vendor 'quack)
(vendor 'jasmin)
(vendor 'forth-mode)
(vendor 'lua-mode)
(vendor 'google-c-style)

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
 '(quack-programs (quote ("/Users/stevej/bin/mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -il r6rs" "mzscheme -il typed-scheme" "mzscheme -M errortrace" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(quack-remap-find-file-bindings-p nil)
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
