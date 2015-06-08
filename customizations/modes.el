;; textmate-minor-mode
(require 'textmate)
(textmate-mode t)

;; ProofGeneral

(load-file "~/.emacs.d/utilities/ProofGeneral-4.2/generic/proof-site.el")

;; haskell-mode
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(setq haskell-program-name "ghci")
(require 'inf-haskell)

;; this trollish wizardry is required for ESS, which I use
;; to integrate Emacs with R.

;;(load "~/.emacs.d/utilities/ess-12.09/lisp/ess-site")
;;(require 'ess-site)
;;(ess-toggle-underscore nil)



;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir "/tmp/")
(setq backup-directory-alist (list (cons "." backup-dir)))

;; browsing
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

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
;;(require 'rnc-mode)

;; gist integration
;;(require 'gist)

;; idris
;;(require 'idris)

;; (add-to-list 'load-path "~/../swank-clojure")
;; (add-to-list 'load-path "~/../slime/contrib")
;; (add-to-list 'load-path "~/../slime")
;; (require 'slime)

;;(autoload 'clojure-mode "clojure-mode" "foo" t)

;; Rust


(defun my-rust-mode-hook ()
  (setq racer-rust-src-path "~/src/github.com/local/src/rustc-1.0.0/src"
        racer-cmd "~/local/src/racer/target/release/racer")
  (add-to-list 'load-path "~/local/src/racer/editors/emacs")
  (require 'racer))

(add-hook 'rust-mode-hook 'flycheck-mode)
(add-hook 'rust-mode-hook 'flycheck-rust-setup)
(add-hook 'rust-mode-hook 'my-rust-mode-hook)

(setq-default tab-always-indent 'complete)


;; incanter
;;(load "~/.emacs.d/vendor/incanter.el")

;; factor integration
;;(load "factor")

;; nxml-mode
;; al3x lol'd at the timestamp. (see if there's a new version)
;; (load "~/.emacs.d/nxml-mode-20041004/rng-auto.el")
;; FIXME: get rng-mode working.

;;(require 'font-lock)

;; thrift-mode
;(require 'thrift-mode)

;; textile-mode
;;(load "textile-mode")

;; SML-mode
;;(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
;;(autoload 'run-sml "sml-proc" "Run an inferior SML process." t)

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

(add-hook 'ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

; smart-tab
;(require 'smart-tab)
;(global-smart-tab-mode 1)
;(setq smart-tab-using-hippie-expand nil)

(setq my-modes
      '(
;;  ("\\.bashrc"  . sh-mode)
;;    ("\\.s$"      . nasm-mode) ;; TODO: support other assemblers
;;    ("\\.asm$"    . nasm-mode)
      ("\\.v"       . coq-mode)
;;    ("\\.js$"     . js-mode)
;;    ("\\.json$"   . js-mode)
;;    ("\\.yml"     . yaml-mode)
;;    ("\\.clj"     . clojure-mode)
;;    ("\\.k"       . scheme-mode)
;;    ("\\.textile" . textile-mode)
      ("\\.sml"     . tuareg-mode)
      ("\\.sig"     . tuareg-mode)
      ("\\.ml[iylp]?" . tuareg-mode)
      ("\\.rb"      . ruby-mode)
;;    ("\\.pp"      . ruby-mode)
;;    ("\\.md"      . markdown-mode)
;;    ("\\.markdown" . markdown-mode)
;;    ("Capfile"    . ruby-mode)
;;    ("capfile"    . ruby-mode)
;;    ("Gemfile"    . ruby-mode)
;;    ("Rakefile"   . ruby-mode)
;;    ("rakefile"   . ruby-mode)
      ("\\.lua"     . lua-mode)
;;    ("\\.fs"      . fsharp-mode)
      ("\\.f$"      . forth-mode))
)

(mapc (lambda (item)
        (add-to-list 'auto-mode-alist item))
      my-modes)

;; allows me to use emacs-client correctly.
;; export EDITOR=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
;;(server-start)


;; tramp
(setq tramp-default-method "ssh")

;; load groc if we have it around
(let* ((groc "/Users/stevej/src/groc/emacs/groc.el"))
  (if (file-exists-p groc)
      (load-file groc)))

;; Deft setup.
;;(setq deft-extension "textile")
;;(setq deft-directory "~/Documents/deft/")
;;(setq deft-text-mode 'textile-mode)

;;(setq rust-indent-unit 2)

;;(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
;;(autoload 'run-coq "coq-inferior" "Run an inferior Coq process." t)
;;(autoload 'run-coq-other-window "coq-inferior"
;;  "Run an inferior Coq process in a new window." t)
;;(autoload 'run-coq-other-frame "coq-inferior"
;;  "Run an inferior Coq process in a new frame." t)


;; use utop for our top-level
;;(autoload 'utop "utop" "Toplevel for OCaml" t)
;;(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
;;(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
;;(add-hook 'typerex-mode-hook 'utop-setup-ocaml-buffer)
;;(require 'utop)

(setq opam-share
       (substring (shell-command-to-string "opam config var share") 0 -1))

;; opam install ocp-indent
(load-file (concat opam-share "/emacs/site-lisp/ocp-indent.el"))
;; opam install ocp-index
(load-file (concat opam-share "/emacs/site-lisp/ocp-index.el"))

;; opam install merlin
(push
 (concat (substring (shell-command-to-string "opam config var share") 0 -1) "/emacs/site-lisp") load-path)
(setq merlin-command (concat (substring (shell-command-to-string "opam config var bin") 0 -1) "/ocamlmerlin"))
(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)

;;(add-hook
;; 'tuareg-mode-hook
;; '(lambda ()
;;    (merlin-mode)
;;    (setq indent-line-function 'ocp-indent-line)
;;    (setq merlin-use-auto-complete-mode t)
;;   (local-set-key (kbd "C-S-<up>") 'merlin-type-enclosing-go-up)
;;    (local-set-key (kbd "C-S-<down>") 'merlin-type-enclosing-go-down)
;;    ))


(setq scheme-program-name "racket -I r5rs")
