;; Author: Justin Zhu <jz@twitter.com>
;;
(require 'autoinsert)
(require 'cl)
(auto-insert-mode)
(setq auto-insert-query nil)

(setq auto-insert-directory (expand-file-name "~/.emacs.d/auto-templates/"))

(setq auto-insert-alist
      '(
        ("Spec\\.scala$" . ["insert.scala" auto-update-scala-source-file])
        ))

(defun filepath-to-package-name (s)
  (reduce (lambda (acc val) (if acc (concat (concat acc val) ".") (when (string= val "scala") ""))) (split-string s "/") :initial-value nil))

(defun auto-update-scala-source-file ()
  (setq bse (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
  (setq bsd (filepath-to-package-name (file-name-directory buffer-file-name)))
  (save-excursion
    ;; replace PPP with package name
    (while (search-forward "PPP" nil t)
           (save-restriction
             (narrow-to-region (match-beginning 0) (match-end 0))
             (replace-match (substring bsd 0 -2) t)
             ))
    )
  (save-excursion
    ;; Replace @@@ with file name
    (while (search-forward "@@@" nil t)
           (save-restriction
             (narrow-to-region (match-beginning 0) (match-end 0))
             (replace-match bse)
             ))
    )
  (save-excursion
    ;; Replace $$$ with src name
    (while (search-forward "$$$" nil t)
           (save-restriction
             (narrow-to-region (match-beginning 0) (match-end 0))
             (replace-match (substring bse 0 -4))
             ))
    )
  )

(defun switch-between-test-and-source ()
  "Switch between a scala test (*Spec) and its corresponding source"
  (interactive)
  ;; grab the base of the current buffer's file name
  (setq bse (file-name-sans-extension buffer-file-name))
  ;; and the extension, converted to lowercase 
  (setq ext (downcase (file-name-extension buffer-file-name)))
  (setq typ (substring bse -4 nil))
  (cond
   ;; first condition - switch to src
   ((equal typ "Spec")
    (setq nfn (replace-regexp-in-string "test" "main" (concat (substring bse 0 -4) ".scala")))

    ;; create directory if doesn't exist
    (setq dirname (file-name-directory nfn))
    (unless (not (file-exists-p dirname)) (make-directory dirname t))
    (find-file nfn)
    )
   ;; second condition - switch to test file
   ((or (equal ext "scala"))
    (setq nfn (replace-regexp-in-string "main" "test" (concat bse "Spec.scala")))
    (setq dirname (file-name-directory nfn))
    (unless (not (file-exists-p dirname)) (make-directory dirname t))
    (find-file nfn)
    )
   )
  )
(add-hook 'scala-mode-hook 
   (lambda () (local-set-key (kbd "C-c s") 'switch-between-test-and-source)))

(provide 'switchy)
