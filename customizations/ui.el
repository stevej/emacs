(setq-default tool-bar-mode nil)
(setq c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(global-auto-revert-mode 1)
;;(windmove-default-keybindings)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; default font is Inconsolata
(setq default-frame-alist
      '((font . "-apple-inconsolata-medium-r-normal--20-140-72-72-m-140-iso10646-1")))

