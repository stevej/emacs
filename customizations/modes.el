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

;; allow us to look at whitespace
(require 'whitespace)
(require 'show-wspace)
(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)

;; relax ng
(require 'rnc-mode)

;; gist integration
(require 'gist)

;; clojure-mode
(require 'clojure-mode)

;; swank-clojure
;;; These calls to vendor are sensitive and should not be moved back to init.el
(vendor 'swank-clojure)
(require 'swank-clojure-autoload)
(swank-clojure-config
 (setq swank-clojure-jar-path "~/local/src/clojure/clojure.jar")
 (setq swank-clojure-extra-classpaths
       (list "~/local/src/clojure/clojure-contrib.jar")))

;; slime
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))))

(vendor 'slime)
(require 'slime)
(slime-setup)
;; end sensitivity.


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
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;; SML-mode
(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
(autoload 'run-sml "sml-proc" "Run an inferior SML process." t)

(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))

;; ruby
;; based on http://www.rubygarden.org/Ruby/page/show/InstallingEmacsExtensions

(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files")

(add-to-list 'auto-mode-alist '("\\.rb$"  . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.pp$"  . ruby-mode)) ; for puppet
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("capfile" . ruby-mode))

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
