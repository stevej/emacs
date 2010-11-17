;; textmate-minor-mode
(require 'textmate)
(textmate-mode t)

;; haskell-mode
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;; browsing
(require 'ido)
(ido-mode t)
;;(setq ido-enable-flex-matching t) ; fuzzy matching is a must have

;; This tab override shouldn't be necessary given ido's default
;; configuration, but minibuffer-complete otherwise dominates the
;; tab binding because of my custom tab-completion-everywhere
;; configuration.
(add-hook 'ido-setup-hook
 (lambda ()
   (define-key ido-completion-map [tab] 'ido-complete)))

;; allow us to look at whitespace
(require 'whitespace)
(require 'show-wspace)
(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)

;; relax ng
(require 'rnc-mode)

;; gist integration
(require 'gist)


;; (add-to-list 'load-path "~/../swank-clojure")
;; (add-to-list 'load-path "~/../slime/contrib")
;; (add-to-list 'load-path "~/../slime")
;; (require 'slime)

(autoload 'clojure-mode "clojure-mode" "foo" t)


;; incanter
;;(load "~/.emacs.d/vendor/incanter.el")

;; factor integration
;;(load "factor")

;; nxml-mode
;; al3x lol'd at the timestamp. (see if there's a new version)
;; (load "~/.emacs.d/nxml-mode-20041004/rng-auto.el")
;; FIXME: get rng-mode working.

;; scala-mode
(require 'scala-mode-auto)
(require 'font-lock)

;; building scala with sbt
(load "~/.emacs.d/support/sbt.el")

;; thrift-mode
(require 'thrift-mode)

;; textile-mode
(load "textile-mode")

;; SML-mode
(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
(autoload 'run-sml "sml-proc" "Run an inferior SML process." t)

;; ruby
;; based on http://www.rubygarden.org/Ruby/page/show/InstallingEmacsExtensions

(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files")

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)))
;; If you have Emacs 19.2x or older, use rubydb2x
(autoload 'rubydb "rubydb3x" "Ruby debugger" t)
;; uncomment the next line if you want syntax highlighting
(add-hook 'ruby-mode-hook 'turn-on-font-lock)

;; Rinari
(require 'rinari)

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

; smart-tab
(require 'smart-tab)
(global-smart-tab-mode 1)
(setq smart-tab-using-hippie-expand nil)

(setq my-modes
  '(("\\.bashrc"  . sh-mode)
    ("\\.js$"     . js-mode)
    ("\\.json$"   . js-mode)
    ("\\.yml"     . yaml-mode)
    ("\\.spde"    . scala-mode)
    ("\\.clj"     . clojure-mode)
    ("\\.textile" . textile-mode)
    ("\\.sml"     . tuareg-mode)
    ("\\.sig"     . tuareg-mode)
    ("\\.ml"      . tuareg-mode)
    ("\\.rb"      . ruby-mode)
    ("\\.pp"      . ruby-mode)
    ("Capfile"    . ruby-mode)
    ("capfile"    . ruby-mode)
    ("\\.lua"     . lua-mode)
    ("\\.f$"      . forth-mode)))

(mapc (lambda (item)
        (add-to-list 'auto-mode-alist item))
      my-modes)

;; Load the ensime lisp code...
(add-to-list 'load-path "/Users/stevej/local/ensime_2.8.1.RC3-0.3.7/elisp/")
(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; MINI HOWTO:
;; Open .scala file. M-x ensime (once per project)

;; allows me to use emacs-client correctly.
;; export EDITOR=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
(server-start)


;; tramp
(setq tramp-default-method "ssh")
