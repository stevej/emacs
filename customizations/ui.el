(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq c-basic-offset 2)
(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(global-auto-revert-mode 1)
;;(windmove-default-keybindings)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(icomplete-mode 1)

; force new frames into existing window
(setq ns-pop-up-frames nil)

;;disable splash screen and startup message
(setq inhibit-startup-message t)


(defun font-existsp (font)
   "Check to see if the named FONT is available."
   (if (null (x-list-fonts font))
       nil t))


(setf s-font-size 230)

(cond
 ((eq window-system nil) nil)
 ((font-existsp "PragmataPro")
  (set-face-attribute 'default nil :height s-font-size :font "PragmataPro")
  )
 ((font-existsp "Menlo")
  (set-face-attribute 'default nil :height s-font-size :font "Menlo")
  )
 ((font-existsp "Consolas")
  (set-face-attribute 'default nil :height s-font-size :font "Consolas")
  )
 ((font-existsp "Inconsolata")
  (set-face-attribute 'default nil :height s-font-size :font "Inconsolata")
  )
 )

(require 'color-theme)
(setq color-theme-is-global t)
(load-file "~/.emacs.d/themes/color-theme-solarized.el")
(color-theme-solarized-light)


(powerline-default-theme)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
'(blink-cursor-mode t)
'(column-number-mode t)
'(show-paren-mode t))


; pretty diff-mode
(custom-set-faces
 '(diff-added ((t (:foreground "#559944"))))
 '(diff-context ((t nil)))
 '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
 '(diff-function ((t (:foreground "#00bbdd"))))
 '(diff-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
 '(diff-hunk-header ((t (:foreground "#fbde2d"))))
 '(diff-nonexistent ((t (:inherit diff-file-header :strike-through nil))))
 '(diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background "#182042"))))
 '(diff-removed ((t (:foreground "#de1923")))))

(setq-default line-spacing 2)
