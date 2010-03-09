;;; tea.el -- Teach emacs about T.
;;; Copyright Olin Shivers (1988)
;;; Please imagine a long, tedious, legalistic 5-page gnu-style copyright
;;; notice appearing here to the effect that you may use this code any
;;; way you like, as long as you don't charge money for it, remove this
;;; notice, or hold me liable for its results.
;;;
;;; 1. Major mode for editing T source: t-mode
;;;    This is just a variant of scheme-mode, tweaked for T.
;;; 2. Major mode for running T in a buffer: run-tea
;;;    This is a customisation of comint-mode.
;;;
;;; Written by Olin Shivers (olin.shivers@cs.cmu.edu). With bits and pieces
;;; lifted from scheme.el, shell.el, clisp.el, newclisp.el, cobol.el, et al..
;;; 8/88
;;; Please send me bug reports, bug fixes, and extensions, so that I can
;;; merge them into the master source.
;;;
;;; Change log at end of file.


;; YOUR .EMACS FILE
;;=============================================================================
;; Some suggestions for your .emacs file.
;;
;; ; If tea.el lives in some non-standard directory, you must tell emacs
;; ; where to get it. This may or may not be necessary.
;; (setq load-path (cons (expand-file-name "~jones/lib/emacs") load-path))
;;
;; ; Autoload run-tea and t-mode from file tea.el
;; (autoload 'run-tea "tea"
;;           "Run an inferior T process."
;;           t)
;;
;; (autoload 't-mode "tea"
;;           "Major mode for editing T source. Just Scheme mode, tuned a bit."
;;           t)
;;
;; ; Files ending in ".t" are T source, so put their buffers in t-mode.
;; (setq auto-mode-alist
;;       (cons '("\\.t$" . t-mode) 
;;	       auto-mode-alist))   
;;
;; ; Define C-c C-t to run my favorite command in inferior T mode:
;; (setq tea-load-hook
;;       '((lambda () (define-key inferior-t-mode-map "\C-c\C-t"
;;                                'favorite-cmd))))

;; ETAGS
;;=============================================================================
;; A suggestion for modifying the etags program so that it knows about T.
;; You should modify the few lines that allow etags to conclude that
;; files that end with ".t" are lisp or scheme source code.  
;; Find a line that looks like
;; /* .scm or .sm or .scheme implies scheme source code */
;; and add a 
;;	     !strcmp (cp + 1, "t") ||
;; suffix check to the following clauses that check filename suffixes.
;; This is already done for some versions of etags. Have a look, or try it 
;; & see.

(setq scheme-mit-dialect nil) ; Give me a break.
(require 'scheme)
(require 'cmushell)


;;; T mode stuff
;;;============================================================================

;;; Note: T mode alters the scheme-mode syntax table and indentation
;;; hooks slightly. If you were using scheme-mode and t-mode simultaneously
;;; this might be a problem, except that the alterations are fairly 
;;; innocuous.

;; This adds [] and {} as matching delimiters. So emacs will treat #[Char 0]
;; or #{Procedure 1 ADD} as an s-exp with a quote sign in front.
(modify-syntax-entry ?[ "(]" scheme-mode-syntax-table)
(modify-syntax-entry ?] ")[" scheme-mode-syntax-table)
(modify-syntax-entry ?{ "(}" scheme-mode-syntax-table)
(modify-syntax-entry ?} "){" scheme-mode-syntax-table)

;; Modify scheme-indent-hook for T.
(put 'labels 'scheme-indent-hook 1)
(put 'block 'scheme-indent-hook 0)
(put 'block0 'scheme-indent-hook 0)
(put 'object 'scheme-indent-hook 1)
(put 'lset 'scheme-indent-hook 1)
(put 'xcase 'scheme-indent-hook 1)
(put 'select 'scheme-indent-hook 1)
(put 'xselect 'scheme-indent-hook 1)
(put 'iterate 'scheme-indent-hook 2)
(put 'cond 'scheme-indent-hook 0)
(put 'xcond 'scheme-indent-hook 0)
(put 'catch 'scheme-indent-hook 1)
(put 'bind 'scheme-indent-hook 1)
(put 'define-operation 'scheme-indent-hook 1)
(put 'operation 'scheme-indent-hook 1)
(put 'object 'scheme-indent-hook 1)
(put 'join 'scheme-indent-hook 0)
(put 'destructure 'scheme-indent-hook 1)
(put 'destructure* 'scheme-indent-hook 1)
(put 'define-integrable 'scheme-indent-hook 1)
(put 'define-constant 'scheme-indent-hook 1)
(put 'define-syntax 'scheme-indent-hook 1)
(put 'let-syntax 'scheme-indent-hook 1)
(put 'define-local-syntax 'scheme-indent-hook 1)
(put 'macro-expander 'scheme-indent-hook 1)
(put 'with-open-streams 'scheme-indent-hook 1)
(put 'with-open-ports 'scheme-indent-hook 1)
(put 'with-input-from-string 'scheme-indent-hook 1)
(put 'with-output-to-string 'scheme-indent-hook 1)
(put 'with-output-width-string 'scheme-indent-hook 1)
(put 'receive 'scheme-indent-hook 2)
(put 'receive-values 'scheme-indent-hook 1)

(defvar t-mode-hook nil
 "*Hook for customising T mode")

(defvar t-mode-map (full-copy-sparse-keymap scheme-mode-map))

(defun t-mode ()
  "Major mode for editing T code. 
This is Scheme mode, slightly tuned for T. Editing commands are similar
to those of Lisp mode.

In addition, if an inferior T process is running, some additional
commands will be defined, for evaluating expressions and controlling
the interpreter, and the state of the process will be displayed in the
modeline of all T buffers.  The names of commands that interact
with the T process start with \"tea-\".  For more information
see the documentation for inferior-t-mode.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{t-mode-map}
Customisation: Entry to this mode runs the hooks on t-mode-hook"
  (interactive)
  (kill-all-local-variables)
  (use-local-map t-mode-map)
  (scheme-mode-variables)
  (setq major-mode 't-mode)
  (setq mode-name "T")
  (run-hooks 't-mode-hook))


;;; INFERIOR T MODE STUFF
;;;============================================================================

(defvar inferior-t-mode-map nil)
(cond ((not inferior-t-mode-map)
       (setq inferior-t-mode-map (full-copy-sparse-keymap comint-mode-map))
       (scheme-mode-commands inferior-t-mode-map)
       (define-key inferior-t-mode-map "\M-\C-x"  'tea-send-definition)
       (define-key inferior-t-mode-map "\C-x\C-e" 'tea-send-last-sexp)
       (define-key inferior-t-mode-map "\C-cl"    'tea-load-file)
       (define-key inferior-t-mode-map "\C-ck"    'tea-compile-file) ;"kompile"
       ))

;; Install the process communication commands in the scheme-mode keymap.
(define-key t-mode-map "\M-\C-x"  'tea-send-definition) ; gnu convention
(define-key t-mode-map "\C-x\C-e" 'tea-send-last-sexp)  ; gnu convention
(define-key t-mode-map "\C-ce"    'tea-send-definition)
(define-key t-mode-map "\C-c\C-e" 'tea-send-definition-and-go)
(define-key t-mode-map "\C-cr"    'tea-send-region)
(define-key t-mode-map "\C-c\C-r" 'tea-send-region-and-go)
(define-key t-mode-map "\C-cc"    'tea-compile-definition)
(define-key t-mode-map "\C-c\C-c" 'tea-compile-definition-and-go)
(define-key t-mode-map "\C-cz"    'switch-to-tea)
(define-key t-mode-map "\C-cl"    'tea-load-file)
(define-key t-mode-map "\C-ck"    'tea-compile-file)

(defvar inferior-t-mode-hook nil
  "*Hook for customising inferior-T mode")

(defun inferior-t-mode ()
  "Major mode for interacting with an inferior T process.

The following commands are available:
\\{inferior-t-mode-map}

A T process can be fired up with M-x run-tea.

Customisation: Entry to this mode runs the hooks on comint-mode-hook and
inferior-t-mode-hook (in that order).

You can send text to the inferior T process from other buffers containing
T source.  
    switch-to-tea switches the current buffer to the T process buffer.
    tea-send-definition sends the current definition to the T process.
    tea-compile-definition compiles the current definition.
    tea-send-region sends the current region to the T process.
    tea-compile-region compiles the current region.

    tea-send-definition-and-go, tea-compile-definition-and-go,
        tea-send-region-and-go, and tea-compile-region-and-go
        switch to the T process buffer after sending their text.
For information on running multiple processes in multiple buffers, see
documentation for variable tea-buffer.

Commands:
Return after the end of the process' output sends the text from the 
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for T; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp "^>+ *") ; Customise in inferior-t-mode-hook
  (scheme-mode-variables)

  (setq major-mode 'inferior-t-mode)
  (setq mode-name "Inferior T")
  (setq mode-line-process '(": %s"))
  (use-local-map inferior-t-mode-map)
  (setq comint-input-filter 'tea-input-filter)
  (setq comint-input-sentinel 'ignore)
  (setq comint-get-old-input 'tea-get-old-input)
  (run-hooks 'inferior-t-mode-hook))

(defun tea-input-filter (str)
  "Don't save anything matching tea-filter-regexp"
  (not (string-match tea-filter-regexp str)))

(defvar tea-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters.")

(defun tea-get-old-input ()
  "Snarf the sexp ending at point"
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

;;; This will break if you have an argument with whitespace, as in
;;; string = "-ab +c -x 'you lose'".
(defun tea-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (tea-args-to-list (substring string (+ 1 where)
					      (length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (tea-args-to-list (substring string pos
					      (length string)))))))))

(defvar tea-program-name "t"
  "*Program invoked by the run-tea command")

;;; Obsolete
(defun tea (&rest foo) (message "Use run-tea"))

(defun run-tea (cmd)
  "Run an inferior T process, input and output via buffer *tea*.
If there is a process already running in *tea*, just switch to that buffer.
With argument, allows you to edit the command line (default is value
of tea-program-name).  Runs the hooks from inferior-t-mode-hook (after the
comint-mode-hook is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
			 (read-string "Run T: " tea-program-name)
			 tea-program-name)))
  (if (not (comint-check-proc "*tea*"))
      (let ((cmdlist (tea-args-to-list cmd)))
	(set-buffer (apply 'make-comint "tea" (car cmdlist)
			   nil (cdr cmdlist)))
	(inferior-t-mode)))
  (setq tea-buffer "*tea*")
  (switch-to-buffer "*tea*"))

(defun tea-send-region (start end)
  "Send the current region to the inferior T process"
  (interactive "r")
  (comint-send-region (tea-proc) start end)
  (comint-send-string (tea-proc) "\n"))

(defun tea-send-definition ()
  "Send the current definition to the inferior T process."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (tea-send-region (point) end))))

(defun tea-send-last-sexp ()
  "Send the previous sexp to the inferior T process."
  (interactive)
  (tea-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun tea-compile-region (start end)
  "Compile the current region in the inferior T process.
\(A BLOCK is wrapped around the region: (BLOCK <region>)"
  (interactive "r")
  (comint-send-string (tea-proc) "(orbit '(block ")
  (comint-send-region (tea-proc) start end)
  (comint-send-string (tea-proc) "))\n"))

(defun tea-compile-definition ()
  "Compile the current definition in the inferior T process."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (tea-compile-region (point) end))))

(defun switch-to-tea (eob-p)
  "Switch to the T process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer tea-buffer)
      (pop-to-buffer tea-buffer)
      (error "No current process buffer. See variable tea-buffer."))
  (cond (eob-p
	 (push-mark)
	 (goto-char (point-max)))))

(defun tea-send-region-and-go (start end)
  "Send the current region to the inferior T process,
and switch to the process buffer."
  (interactive "r")
  (tea-send-region start end)
  (switch-to-tea t))

(defun tea-send-definition-and-go ()
  "Send the current definition to the inferior T process, 
and switch to the process buffer."
  (interactive)
  (tea-send-definition)
  (switch-to-tea t))

(defun tea-compile-region-and-go (start end)
  "Compile the current region in the inferior T process, 
and switch to process buffer."
  (interactive "r")
  (tea-compile-region start end)
  (switch-to-tea t))

(defun tea-compile-definition-and-go ()
  "Compile the current definition in the inferior T process,
and switch to process buffer."
  (interactive)
  (tea-compile-definition)
  (switch-to-tea t))

(defvar tea-source-modes '(t-mode)
  "*Used to determine if a buffer contains T source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a T source file by tea-load-file and tea-compile-file.
Used by these commands to determine defaults.")

(defvar tea-prev-l/c-dir/file nil
  "Caches the (directory . file) pair used in the last tea-load-file or
tea-compile-file command. Used for determining the default in the next one.")

(defun tea-load-file (file-name)
  "Load a T file into the inferior T process."
  (interactive (comint-get-source "Load T file: " tea-prev-l/c-dir/file
				  tea-source-modes t)) ; T because LOAD needs
                                                       ; an exact name.
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq tea-prev-l/c-dir/file (cons (file-name-directory    file-name)
				    (file-name-nondirectory file-name)))
  (comint-send-string (tea-proc) (concat "(load \""
					 file-name
					 "\"\)\n"))
  (switch-to-tea t))

(defun tea-compile-file (file-name)
  "Compile a T file in the inferior T process."
  (interactive (comint-get-source "Compile T file: " tea-prev-l/c-dir/file
				  tea-source-modes
				  nil)) ; NIL because COMPILE doesn't
                                        ; need an exact name.
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq tea-prev-l/c-dir/file (cons (file-name-directory    file-name)
				    (file-name-nondirectory file-name)))
  (comint-send-string (tea-proc) (concat "(compile-file \""
					 file-name
					 "\"\)\n"))
  (switch-to-tea t))


;;; This helps when you run more than one T process at a time.

;;; If we're in some inferior T buffer, return its process,
;;; even if the buffer's been renamed. If we're elsewhere, assume
;;; the standard process named "tea".
(defun tea-proc ()
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and proc (eq major-mode 'inferior-t-mode)) proc
	(get-process "tea"))))

(defvar tea-buffer nil "*The current T process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
Tea.el supports, in a fairly simple fashion, running multiple T
processes. To run multiple T processes, you start the first up with
\\[run-tea]. It will be in a buffer named *tea*. Rename this buffer
with \\[rename-buffer]. You may now start up a new process with another
\\[run-tea]. It will be in a new buffer, named *tea*. You can
switch between the different process buffers with \\[switch-to-buffer].

Commands that send text from source buffers to T processes --
like tea-send-definition or tea-compile-region -- have to choose a
process to send to, when you have more than one T process around. This
is determined by the global variable tea-buffer. Suppose you
have three inferior T's running:
    Buffer	Process
    foo		tea
    bar		tea<2>
    *tea*       tea<3>
If you do a \\[tea-send-definition-and-go] command on some T source code,
what process do you send it to?

- If you're in a process buffer (foo, bar, or *tea*), 
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer tea-buffer.
This process selection is performed by function tea-proc.

Whenever \\[run-tea] fires up a new process, it resets tea-buffer
to be the new process's buffer. If you only run one process, this will
do the right thing. If you run multiple processes, you can change
tea-buffer to another process buffer with \\[set-variable].

More sophisticated approaches are, of course, possible. If you find youself
needing to switch back and forth between multiple processes frequently,
you may wish to consider ilisp.el, a larger, more sophisticated package
for running inferior Lisp and Scheme processes. The approach taken here is
for a minimal, simple implementation. Feel free to extend it.")

(defun tea-proc ()
  "Returns the current T process. See variable tea-buffer."
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-t-mode)
				      (current-buffer)
				      tea-buffer))))
    (or proc
	(error "No current process. See variable tea-buffer"))))


;;; Do the user's customisation...

(defvar tea-load-hook nil
  "This hook is run when tea is loaded in.
This is a good place to put keybindings.")
	
(run-hooks 'tea-load-hook)

;;; CHANGE LOG
;;; ===========================================================================
;;; 8/88 Olin
;;; Created.
;;;
;;; 2/15/89 Olin
;;; Removed -emacs flag from process invocation. It's only useful for
;;; cscheme, and makes cscheme assume it's running under xscheme.el,
;;; which messes things up royally. A bug.
;;;
;;; 5/22/90 Olin
;;; Upgraded to use comint-send-string and comint-send-region.
;;; - run-tea now offers to let you edit the command line if
;;;   you invoke it with a prefix-arg. M-x tea is redundant, and
;;;   has been removed.
;;; - Explicit references to process "tea" have been replaced with
;;;   (tea-proc). This allows better handling of multiple process bufs.
;;; - Added tea-send-last-sexp, bound to C-x C-e. A gnu convention.
;;; - Have not added process query facility a la cmulisp.el's lisp-show-arglist
;;;   and friends, but interested hackers might find a useful application
;;;   of this facility.
