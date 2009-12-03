;; gist.el --- Emacs integration for gist.github.com
;; Copyright (C) 2008  Christian Neukirchen <purl.org/net/chneukirchen>
;; Licensed under the same terms as Emacs.

;; Version: 0.2
;; 21jul2008  +chris+

;; Ideas: fetch & fork

(defun gist-region (begin end)
  "Post the current region as a new paste at gist.github.com
Copies the URL into the kill ring."
  (interactive "r")
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (name (file-name-nondirectory file))
         (ext (or (file-name-extension file) "txt"))
         (output (generate-new-buffer " *gist*")))
    (shell-command-on-region
     begin end
     (format (concat "curl -sS "
                     "-F 'file_ext[gistfile1]=.%s' "
                     "-F 'file_name[gistfile1]=%s' "
                     "-F 'file_contents[gistfile1]=<-' "
                     "http://gist.github.com/gists &") ext name)
     output)
    (with-current-buffer output
      (re-search-backward "href=\"\\(.*\\)\"")
      (message "Paste created: %s" (match-string 1))
      (kill-new (match-string 1)))
    (kill-buffer output)))

(defun gist-buffer ()
  "Post the current buffer as a new paste at gist.github.com.
Copies the URL into the kill ring."
  (interactive)
  (gist-region (point-min) (point-max)))

(provide 'gist)
;;; gist.el ends here.
