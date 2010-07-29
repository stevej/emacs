; save minibuffer history across sessions
(setq savehist-file "~/.emacs.d/.savehist")
(savehist-mode 1)

; nicer naming of buffers with identical names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(require 'midnight)
(setq midnight-period (* 3 24 60 60)); 3 days

;; inserts over highlighted regions when you press a key rather than writing at the point.
;; also allows delete to work on selections made with a keyboard.
(delete-selection-mode 1)
;; I'm not into scrollbars
(scroll-bar-mode nil)