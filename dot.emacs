(require 'cl)

(load-file "~/.emacs.d/load-directory.el")
(mapcar 'load-directory '("~/.emacs.d/utilities"))


(vendor 'haskell-mode)
(vendor 'textmate)

(mapcar 'load-directory '("~/.emacs.d/customizations"))

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

;; version control
(load "~/.emacs.d/vendor/magit/magit.el")

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/utilities/ert")
(add-to-list 'load-path "~/.emacs.d/utilities/jump")

;; load configgy-mode
(add-to-list 'load-path "~/.emacs.d/vendor/configgy-mode")
(require 'configgy-mode)

;; allow us to look at whitespace
(require 'whitespace)
(require 'show-wspace)
(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)

;; relax ng
(require 'rnc-mode)

;; gist integration
(require 'gist)

;; clojure-mode
(add-to-list 'load-path "~/.emacs.d/vendor/clojure-mode")
(require 'clojure-mode)

;; swank-clojure
(add-to-list 'load-path "~/.emacs.d/vendor/swank-clojure")
(require 'swank-clojure-autoload)
(swank-clojure-config
 (setq swank-clojure-jar-path "~/local/src/clojure/clojure.jar")
 (setq swank-clojure-extra-classpaths
       (list "~/local/src/clojure/clojure-contrib.jar")))

;; slime
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))))

(add-to-list 'load-path "~/.emacs.d/vendor/slime")
(require 'slime)
(slime-setup)

;; incanter
(load "~/.emacs.d/vendor/incanter.el")

;; factor integration
;;(load "factor")

;; nxml-mode
(load "~/.emacs.d/nxml-mode-20041004/rng-auto.el")

;; scala-mode
(add-to-list 'load-path "~/.emacs.d/scala-mode")
(require 'scala-mode-auto)
(require 'font-lock)

;; building scala with sbt
(load "~/.emacs.d/support/sbt.el")

;; thrift-mode
(add-to-list 'load-path "~/.emacs.d/thrift-mode")
(load "thrift")
(require 'thrift-mode)

;; textile-mode
(load "textile-mode")
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;; ocaml tuareg mode
(add-to-list 'load-path "~/.emacs.d/tuareg-mode-1.45.4/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; append-tuareg.el - Tuareg quick installation: Append this file to .emacs.

(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

(if (and (boundp 'window-system) window-system)
    (when (string-match "XEmacs" emacs-version)
        (if (not (and (boundp 'mule-x-win-initted) mule-x-win-initted))
            (require 'sym-lock))
        (require 'font-lock)))

;; SML-mode
(add-to-list 'load-path "~/.emacs.d/sml-mode-4.0/")
(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
(autoload 'run-sml "sml-proc" "Run an inferior SML process." t)

(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))

;; ruby
;; based on http://www.rubygarden.org/Ruby/page/show/InstallingEmacsExtensions

(add-to-list 'load-path "~/.emacs.d/ruby")

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
(add-to-list 'load-path "~/.emacs.d/vendor/rinari")
(require 'rinari)

;; ant helper
(defvar ant-command-history nil
  "Ant command history variable")

(defun ant(&optional args)
  "Runs ant in the current project. Starting at the directory
where the file being visited resides, a search is made for
build.xml recursively. A maven command is made from the first
directory where the build.xml file is found is then displayed in
the minibuffer. The command can be edited as needed and then
executed. Errors are navigate to as in any other compile mode"
  (interactive)
  (let ((fn (buffer-file-name)))
    (let ((dir (file-name-directory fn)))
      (while (and (not (file-exists-p (concat dir "/build.xml")))
                  (not (equal dir (file-truename (concat dir "/..")))))
        (setf dir (file-truename (concat dir "/.."))))
      (if (not (file-exists-p (concat dir "/build.xml")))
          (message "No build.xml found")
        (compile (read-from-minibuffer "Command: "
                                       (concat "ant -emacs -f "
                                       dir "/build.xml test") nil
                                       nil
                                       'ant-command-history))))))


;; full-screen-mode
(defun mac-toggle-max-window ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil

                                           'fullboth)))

