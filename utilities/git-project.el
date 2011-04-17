(defvar git-grep-history '())
(defvar git-grep-command "git grep -nH")
(defvar git-tree-command "git rev-parse --show-cdup")

;; from http://www.emacswiki.org/emacs/ElispCookbook
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun git-tree-root ()
  (let ((root (chomp (shell-command-to-string git-tree-command))))
    (if (string= root "") "." root)))


(defun git-grep-default-command ()
  (let ((grep-command git-grep-command)
        (grep-history git-grep-history))
    (grep-default-command)))

(defun git-grep-default-command ()
  (let ((tag-default (shell-quote-argument (grep-tag-default)))
        (file-ext (and buffer-file-name (file-name-extension buffer-file-name))))
    (git-grep-tree-command
     tag-default
     (if file-ext (concat "'" "*." file-ext "'") nil))))

(defun git-grep-tree-command (&optional expr file-ext)
  (cons
   (concat
    git-grep-command " "
    (or expr "")
    (if file-ext (concat " -- " file-ext) ""))
   (+ 2 (length git-grep-command))))

(defun git-grep-tree (command-args)
  (interactive
   (progn
     (let ((default (git-grep-default-command)))
       (list (read-shell-command
              "Run git-grep (like this): "
              (if current-prefix-arg
                  default (git-grep-tree-command))
              'git-grep-history
              nil)))))

  (compilation-start
   (concat "cd " (git-tree-root) ";" command-args)
   'grep-mode))

(defun last-component (str sep)
  (car (last (split-string str sep t))))

(defun git-find-file ()
  (interactive)
  (let* ((root (git-tree-root))
         (expanded-root (expand-file-name root))
         (dir (last-component expanded-root "/")))
    (find-file
     (concat
      expanded-root "/"
      (ido-completing-read
       (concat dir ": ")
       (split-string
        (shell-command-to-string
         (concat "cd " root ";" "git ls-files")))
       "\n" t)))))
