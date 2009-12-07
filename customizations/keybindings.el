;; porkrind sees the delete key as kp-delete but binds it to delete-backward-char.
(global-set-key (kbd "<kp-delete>") 'delete-char)

; stuff for pairing with al3x
(keyboard-translate ?\C-t ?\C-x)

; To be able to M-x without meta - yes, this overwrites exiting but
; I don't care because I quit Apple style
(global-set-key (kbd "C-x C-c") 'execute-extended-command)
(global-set-key (kbd "C-x c") 'execute-extended-command)
(global-set-key (kbd "C-x m") 'execute-extended-command)

; open file
(global-set-key [(super o)] 'find-file)

; use full-ack for Find
(global-set-key [(super F)] 'ack)

; buffer switching
(global-set-key [(super {)] 'previous-buffer)
(global-set-key [(super })] 'next-buffer)

; window switching
(global-set-key (kbd "s-`") 'other-window)

; close window
(global-set-key [(super w)]
  (lambda ()
    (interactive)
    (kill-buffer (current-buffer))))
