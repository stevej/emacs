;; textmate-minor-mode
(textmate-mode)

;; haskell-mode
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

;; customizations
(setq-default tool-bar-mode nil)
(setq c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(global-auto-revert-mode 1)
;;(windmove-default-keybindings)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq default-frame-alist
      '((font . "-apple-inconsolata-medium-r-normal--20-140-72-72-m-140-iso10646-1")))

;; porkrind sees the delete key as kp-delete but binds it to delete-backward-char.
(global-set-key (kbd "<kp-delete>") 'delete-char)

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir
 (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;; .bashrc should open in sh mode
(setq auto-mode-alist (cons '("\\.bashrc" . sh-mode) auto-mode-alist))

;; browsing
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have

;; This tab override shouldn't be necessary given ido's default
;; configuration, but minibuffer-complete otherwise dominates the
;; tab binding because of my custom tab-completion-everywhere
;; configuration.
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [tab] 'ido-complete)))

