(setq-default tool-bar-mode nil)
(setq c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(global-auto-revert-mode 1)

(setq default-frame-alist
      '((font . "-apple-bitstream vera sans mono-medium-r-normal--18-140-72-72-m-140-iso10646-1")))

;; kill tabs.
(add-to-list 'load-path "~/.emacs.d")
(require 'show-wspace)
(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)

;; scala-mode
(add-to-list 'load-path "~/.emacs.d/scala-mode")
(require 'scala-mode-auto)
(require 'font-lock)

;; thrift-mode
(add-to-list 'load-path "~/.emacs.d/thrift-mode")
(load "thrift")
(require 'thrift-mode)

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

