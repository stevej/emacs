;;; sml-proc.el --- Comint based interaction mode for Standard ML.

;; Copyright (C) 1999,2000,03,04  Stefan Monnier
;; Copyright (C) 1994-1997  Matthew J. Morley
;; Copyright (C) 1989       Lars Bo Nielsen

;; 1.21
;; 2004/11/15 03:51:19

;; ====================================================================

;; This file is not part of GNU Emacs, but it is distributed under the
;; same conditions.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 0139, USA.
;; (See sml-mode.el for HISTORY.) 

;; ====================================================================

;; [MJM 10/94] Separating this from sml-mode means sml-mode will run
;; under 18.59 (or anywhere without comint, if there are such places).
;; See sml-mode.el for further information.

;;; Commentary:

;; Inferior-sml-mode is for interacting with an ML process run under
;; emacs. This uses the comint package so you get history, expansion,
;; backup and all the other benefits of comint. Interaction is
;; achieved by M-x run-sml which starts a sub-process under emacs. You may
;; need to set this up for autoloading in your .emacs:

;; (autoload 'run-sml "sml-proc" "Run an inferior ML process." t)

;; Exactly what process is governed by the variable sml-program-name
;; -- just "sml" by default. If you give a prefix argument (C-u M-x
;; run-sml) you will be prompted for a different program to execute from
;; the default -- if you just hit RETURN you get the default anyway --
;; along with the option to specify any command line arguments. Once
;; you select the ML program name in this manner, it remains the
;; default (unless you set in a hook, or otherwise).

;; NOTE: inferior-sml-mode-hook is run AFTER the ML program has been
;; launched. inferior-sml-load-hook is run only when sml-proc.el is
;; loaded into Emacs.

;; When running an ML process some further key-bindings are effective
;; in sml-mode buffer(s). C-c C-s (switch-to-sml) will split the
;; screen into two windows if necessary and place you in the ML
;; process buffer. In the interaction buffer, C-c C-s is bound to the
;; `sml' command by default (in case you need to restart).

;; C-c C-l (sml-load-file) will load an SML source file into the
;; inferior process, C-c C-r (sml-send-region) will send the current
;; region of text to the ML process, etc. Given a prefix argument to
;; these commands will switch you from the SML buffer to the ML
;; process buffer as well as sending the text. If you get errors
;; reported by the compiler, C-x ` (next-error) will step through
;; the errors with you.

;; NOTE. There is only limited support for this as it obviously
;; depends on the compiler's error messages being recognised by the
;; mode. Error reporting is currently only geared up for SML/NJ,
;; Moscow ML, and Poly/ML.  For other compilers, add the relevant
;; regexp to sml-error-regexp-alist and send it to me.

;; To send pieces of code to the underlying compiler, we never send the text
;; directly but use a temporary file instead.  This breaks if the compiler
;; does not understand `use', but has the benefit of allowing better error
;; reporting.

;; Bugs:

;; Todo:

;; - Keep improving `sml-compile'.
;; - ignore warnings (if requested) for next-error

;;; Code:

(eval-when-compile (require 'cl))
(require 'sml-mode)
(require 'sml-util)
(require 'comint)
(require 'compile)

(defgroup sml-proc ()
  "Interacting with an SML process."
  :group 'sml)

(defcustom sml-program-name "sml"
  "*Program to run as ML."
  :group 'sml-proc
  :type '(string))

(defcustom sml-default-arg ""
  "*Default command line option to pass, if any."
  :group 'sml-proc
  :type '(string))

(defcustom sml-host-name ""
  "*Host on which to run ML."
  :group 'sml-proc
  :type '(string))

(defcustom sml-config-file "~/.smlproc.sml"
  "*File that should be fed to the ML process when started."
  :group 'sml-proc
  :type '(string))

(defcustom sml-compile-command "CM.make()"
  "The command used by default by `sml-compile'.
See also `sml-compile-commands-alist'.")

(defcustom sml-compile-commands-alist
  '(("CMB.make()" . "all-files.cm")
    ("CMB.make()" . "pathconfig")
    ("CM.make()" . "sources.cm")
    ("use \"load-all\"" . "load-all"))
  "*Commands used by default by `sml-compile'.
Each command is associated with its \"main\" file.
It is perfectly OK to associate several files with a command or several
commands with the same file.")

(defvar inferior-sml-mode-hook nil
  "*This hook is run when the inferior ML process is started.
All buffer local customisations for the interaction buffers go here.")

(defvar sml-error-overlay nil
  "*Non-nil means use an overlay to highlight errorful code in the buffer.
The actual value is the name of a face to use for the overlay.
Instead of setting this variable to 'region, you can also simply keep
it NIL and use (transient-mark-mode) which will provide similar
benefits (but with several side effects).")

(defvar sml-buffer nil
  "*The current ML process buffer.

MULTIPLE PROCESS SUPPORT (Whoever wants multi-process support anyway?)
=====================================================================
`sml-mode' supports, in a fairly simple fashion, running multiple ML
processes.  To run multiple ML processes, you start the first up with
\\[sml].  It will be in a buffer named *sml*.  Rename this buffer with
\\[rename-buffer].  You may now start up a new process with another
\\[sml].  It will be in a new buffer, named *sml*.  You can switch
between the different process buffers with \\[switch-to-buffer].

NB *sml* is just the default name for the buffer.  It actually gets
it's name from the value of `sml-program-name' -- *poly*, *smld*,...

If you have more than one ML process around, commands that send text
from source buffers to ML processes -- like `sml-send-function' or
`sml-send-region' -- have to choose a process to send it to.  This is
determined by the global variable `sml-buffer'.  Suppose you have three
inferior ML's running:
    Buffer      Process
    sml         #<process sml>
    mosml       #<process mosml>
    *sml*       #<process sml<2>>
If you do a \\[sml-send-function] command on some ML source code, 
what process do you send it to?

- If you're in a process buffer (sml, mosml, or *sml*), you send it to
  that process (usually makes sense only to `sml-load-file').
- If you're in some other buffer (e.g., a source file), you send it to
  the process attached to buffer `sml-buffer'.

This process selection is performed by function `sml-proc' which looks
at the value of `sml-buffer' -- which must be a Lisp buffer object, or
a string \(or nil\).

Whenever \\[sml] fires up a new process, it resets `sml-buffer' to be
the new process's buffer.  If you only run one process, this will do
the right thing.  If you run multiple processes, you can change
`sml-buffer' to another process buffer with \\[set-variable], or
use the command \\[sml-buffer] in the interaction buffer of choice.")


;;; ALL STUFF THAT DEFAULTS TO THE SML/NJ COMPILER (0.93)

(defvar sml-use-command "use \"%s\""
  "*Template for loading a file into the inferior ML process.
Set to \"use \\\"%s\\\"\" for SML/NJ or Edinburgh ML; 
set to \"PolyML.use \\\"%s\\\"\" for Poly/ML, etc.")

(defvar sml-cd-command "OS.FileSys.chDir \"%s\""
  "*Command template for changing working directories under ML.
Set this to nil if your compiler can't change directories.

The format specifier \"%s\" will be converted into the directory name
specified when running the command \\[sml-cd].")

(defcustom sml-prompt-regexp "^[-=>#] *"
  "*Regexp used to recognise prompts in the inferior ML process."
  :group 'sml-proc
  :type '(regexp))

(defvar sml-error-regexp-alist
  `( ;; Poly/ML messages
    ("^\\(Error\\|Warning:\\) in '\\(.+\\)', line \\([0-9]+\\)" 2 3)
    ;; Moscow ML
    ("^File \"\\([^\"]+\\)\", line \\([0-9]+\\)\\(-\\([0-9]+\\)\\)?, characters \\([0-9]+\\)-\\([0-9]+\\):" 1 2 5)
    ;; SML/NJ:  the file-pattern is anchored to avoid
    ;; pathological behavior with very long lines.
    ("^[-= ]*\\(.*[^\n)]\\)\\( (.*)\\)?:\\([0-9]+\\)\\.\\([0-9]+\\)\\(-\\([0-9]+\\)\\.\\([0-9]+\\)\\)? \\(Error\\|Warnin\\(g\\)\\): .*" 1
     ,@(if (fboundp 'compilation-fake-loc) ;New compile.el.
	   '((3 . 6) (4 . 7) (9))
	 '(sml-make-error 3 4 6 7)))
    ;; SML/NJ's exceptions:  see above.
    ("^ +\\(raised at: \\)?\\(.+\\):\\([0-9]+\\)\\.\\([0-9]+\\)\\(-\\([0-9]+\\)\\.\\([0-9]+\\)\\)" 2
     ,@(if (fboundp 'compilation-fake-loc) ;New compile.el.
	   '((3 . 6) (4 . 7))
	 '(sml-make-error 3 4 6 7))))
  "Alist that specifies how to match errors in compiler output.
See `compilation-error-regexp-alist' for a description of the format.")

;; font-lock support
(defconst inferior-sml-font-lock-keywords
  `(;; prompt and following interactive command
    (,(concat "\\(" sml-prompt-regexp "\\)\\(.*\\)")
     (1 font-lock-prompt-face)
     (2 font-lock-command-face keep))
    ;; CM's messages
    ("^\\[\\(.*GC #.*\n\\)*.*\\]" . font-lock-comment-face)
    ;; SML/NJ's irritating GC messages
    ("^GC #.*" . font-lock-comment-face)
    ;; error messages
    ,@(mapcar (lambda (ra) (cons (car ra) 'font-lock-warning-face))
	      sml-error-regexp-alist))
  "Font-locking specification for inferior SML mode.")

(defface font-lock-prompt-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight prompts."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-prompt-face 'font-lock-prompt-face
  "Face name to use for prompts.")

(defface font-lock-command-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight interactive commands."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-command-face 'font-lock-command-face
  "Face name to use for interactive commands.")

(defconst inferior-sml-font-lock-defaults
  '(inferior-sml-font-lock-keywords nil nil nil nil))


;;; CODE

(defmap inferior-sml-mode-map
  '(("\C-c\C-s"	. run-sml)
    ("\C-c\C-l"	. sml-load-file)
    ("\t"	. comint-dynamic-complete))
  "Keymap for inferior-sml mode"
  :inherit comint-mode-map
  :group 'sml-proc)


;; buffer-local

(defvar sml-temp-file nil)
;;(defvar sml-error-file nil)             ; file from which the last error came
(defvar sml-error-cursor nil)           ;   ditto

(defun sml-proc-buffer ()
  "Return the current ML process buffer.
or the current buffer if it is in `inferior-sml-mode'.  Raises an error
if the variable `sml-buffer' does not appear to point to an existing
buffer."
  (or (and (eq major-mode 'inferior-sml-mode) (current-buffer))
      (and sml-buffer
	   (let ((buf (get-buffer sml-buffer)))
	     ;; buffer-name returns nil if the buffer has been killed
	     (and buf (buffer-name buf) buf)))
      ;; no buffer found, make a new one
      (save-excursion (call-interactively 'run-sml))))

(defun sml-buffer (echo)
  "Make the current buffer the current `sml-buffer' if that is sensible.
Lookup variable `sml-buffer' to see why this might be useful.
If prefix argument ECHO is set, then it only reports on the current state."
  (interactive "P")
  (when (not echo)
    (setq sml-buffer
	  (if (eq major-mode 'inferior-sml-mode) (current-buffer)
	    (read-buffer "Set ML process buffer to: " nil t))))
  (message "ML process buffer is now %s."
	   (or (ignore-errors (buffer-name (get-buffer sml-buffer)))
	       "undefined")))

(defun sml-proc ()
  "Return the current ML process.  See variable `sml-buffer'."
  (assert (eq major-mode 'inferior-sml-mode))
  (or (get-buffer-process (current-buffer))
      (progn (call-interactively 'run-sml)
	     (get-buffer-process (current-buffer)))))

(define-derived-mode inferior-sml-mode comint-mode "Inferior-SML"
  "Major mode for interacting with an inferior ML process.

The following commands are available:
\\{inferior-sml-mode-map}

An ML process can be fired up (again) with \\[sml].

Customisation: Entry to this mode runs the hooks on `comint-mode-hook'
and `inferior-sml-mode-hook' (in that order).

Variables controlling behaviour of this mode are

`sml-program-name' (default \"sml\")
    Program to run as ML.

`sml-use-command' (default \"use \\\"%s\\\"\")
    Template for loading a file into the inferior ML process.

`sml-cd-command' (default \"System.Directory.cd \\\"%s\\\"\")
    ML command for changing directories in ML process (if possible).

`sml-prompt-regexp' (default \"^[\\-=] *\")
    Regexp used to recognise prompts in the inferior ML process.

You can send text to the inferior ML process from other buffers containing
ML source.
    `switch-to-sml' switches the current buffer to the ML process buffer.
    `sml-send-function' sends the current *paragraph* to the ML process.
    `sml-send-region' sends the current region to the ML process.

    Prefixing the sml-send-<whatever> commands with \\[universal-argument]
    causes a switch to the ML process buffer after sending the text.

For information on running multiple processes in multiple buffers, see
documentation for variable `sml-buffer'.

Commands:
RET after the end of the process' output sends the text from the
    end of process to point.
RET before the end of the process' output copies the current line
    to the end of the process' output, and sends it.
DEL converts tabs to spaces as it moves back.
TAB file name completion, as in shell-mode, etc.."
  (setq comint-prompt-regexp sml-prompt-regexp)
  (sml-mode-variables)

  (set (make-local-variable 'font-lock-defaults)
       inferior-sml-font-lock-defaults)
  ;; For sequencing through error messages:
  (set (make-local-variable 'sml-error-cursor) (point-max-marker))
  (set-marker-insertion-type sml-error-cursor nil)

  ;; Compilation support (used for `next-error').
  ;; The keymap of compilation-minor-mode is too unbearable, so we
  ;; just can't use the minor-mode if we can't override the map.
  (when (boundp 'minor-mode-overriding-map-alist)
    (set (make-local-variable 'compilation-error-regexp-alist)
	 sml-error-regexp-alist)
    (compilation-minor-mode 1)
    ;; Eliminate compilation-minor-mode's map.
    (add-to-list 'minor-mode-overriding-map-alist
		 (cons 'compilation-minor-mode (make-sparse-keymap)))
    ;; I'm sure people might kill me for that
    (setq compilation-error-screen-columns nil)
    (make-local-variable 'sml-endof-error-alist))
    ;;(make-local-variable 'sml-error-overlay)

  (setq mode-line-process '(": %s")))

;;; FOR RUNNING ML FROM EMACS

;;;###autoload
(autoload 'run-sml "sml-proc" nil t)
(defalias 'run-sml 'sml-run)
(defun sml-run (cmd arg &optional host)
  "Run the program CMD with given arguments ARG.
The command is run in buffer *CMD* using mode `inferior-sml-mode'.
If the buffer already exists and has a running process, then
just go to this buffer.

This updates `sml-buffer' to the new buffer.
You can have several inferior M(or L process running, but only one (> s
current one -- given by `sml-buffer' (qv).

If a prefix argument is used, the user is also prompted for a HOST
on which to run CMD using `remote-shell-program'.

\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive
   (list
    (read-string "ML command: " sml-program-name)
    (if (or current-prefix-arg (> (length sml-default-arg) 0))
	(read-string "Any args: " sml-default-arg)
      sml-default-arg)
    (if (or current-prefix-arg (> (length sml-host-name) 0))
	(read-string "On host: " sml-host-name)
      sml-host-name)))
  (let* ((pname (file-name-nondirectory cmd))
         (args (if (equal arg "") () (split-string arg)))
	 (file (when (and sml-config-file (file-exists-p sml-config-file))
		 sml-config-file)))
    ;; and this -- to keep these as defaults even if
    ;; they're set in the mode hooks.
    (setq sml-program-name cmd)
    (setq sml-default-arg arg)
    (setq sml-host-name host)
    ;; For remote execution, use `remote-shell-program'
    (when (> (length host) 0)
      (setq args (list* host "cd" default-directory ";" cmd args))
      (setq cmd remote-shell-program))
    ;; go for it
    (let ((exec-path (if (file-name-directory cmd)
			 ;; If the command has slashes, make sure we
			 ;; first look relative to the current directory.
			 ;; Emacs-21 does it for us, but not Emacs-20.
			 (cons default-directory exec-path) exec-path)))
      (setq sml-buffer (apply 'make-comint pname cmd file args)))

    (pop-to-buffer sml-buffer)
    ;;(message (format "Starting \"%s\" in background." pname))
    (inferior-sml-mode)
    (goto-char (point-max))
    sml-buffer))

(defun switch-to-sml (eobp)
  "Switch to the ML process buffer.
Move point to the end of buffer unless prefix argument EOBP is set."
  (interactive "P")
  (pop-to-buffer (sml-proc-buffer))
  (unless eobp
    (push-mark (point) t)
    (goto-char (point-max))))

;; Fakes it with a "use <temp-file>;" if necessary.

(defun sml-send-region (start end &optional and-go)
  "Send current region START..END to the inferior ML process.
Prefix AND-GO argument means switch-to-sml afterwards.

The region is written out to a temporary file and a \"use <temp-file>\" command
is sent to the compiler.
See variables `sml-use-command'."
  (interactive "r\nP")
  (if (= start end)
      (message "The region is zero (ignored)")
    (let* ((buf (sml-proc-buffer))
	   (marker (copy-marker start))
	   (tmp (make-temp-file "sml")))
      (write-region start end tmp nil 'silently)
      (with-current-buffer buf
	(when sml-temp-file
	  (ignore-errors (delete-file (car sml-temp-file)))
	  (set-marker (cdr sml-temp-file) nil))
	(setq sml-temp-file (cons tmp marker))
	(sml-send-string (format sml-use-command tmp) nil and-go)))))

;; This is quite bogus, so it isn't bound to a key by default.
;; Anyone coming up with an algorithm to recognise fun & local
;; declarations surrounding point will do everyone a favour!

(defun sml-send-function (&optional and-go)
  "Send current paragraph to the inferior ML process. 
With a prefix argument AND-GO switch to the sml buffer as well 
\(cf. `sml-send-region'\)."
  (interactive "P")
  (save-excursion
    (sml-mark-function)
    (sml-send-region (point) (mark)))
  (if and-go (switch-to-sml nil)))

(defvar sml-source-modes '(sml-mode)
  "*Used to determine if a buffer contains ML source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered an ML source file by `sml-load-file'.  Used by these commands
to determine defaults.")

(defun sml-send-buffer (&optional and-go)
  "Send buffer to inferior shell running ML process. 
With a prefix argument AND-GO switch to the sml buffer as well
\(cf. `sml-send-region'\)."
  (interactive "P")
  (if (memq major-mode sml-source-modes)
    (sml-send-region (point-min) (point-max) and-go)))

;; Since sml-send-function/region take an optional prefix arg, these
;; commands are redundant. But they are kept around for the user to
;; bind if she wishes, since its easier to type C-c r than C-u C-c C-r.

(defun sml-send-region-and-go (start end)
  "Send current region START..END to the inferior ML process, and go there."
  (interactive "r")
  (sml-send-region start end t))

(defun sml-send-function-and-go ()
  "Send current paragraph to the inferior ML process, and go there."
  (interactive)
  (sml-send-function t))

;;; LOADING AND IMPORTING SOURCE FILES:

(defvar sml-prev-dir/file nil
  "Cache for (DIRECTORY . FILE) pair last.
Set in `sml-load-file' and `sml-cd' commands.
Used to determine the default in the next `ml-load-file'.")

(defun sml-load-file (&optional and-go)
  "Load an ML file into the current inferior ML process. 
With a prefix argument AND-GO switch to sml buffer as well.

This command uses the ML command template `sml-use-command' to construct
the command to send to the ML process\; a trailing \"\;\\n\" will be added
automatically."
  (interactive "P")
  (let ((file (car (comint-get-source
		    "Load ML file: " sml-prev-dir/file sml-source-modes t))))
    (with-current-buffer (sml-proc-buffer)
      ;; Check if buffer needs saved. Should (save-some-buffers) instead?
      (comint-check-source file)
      (setq sml-prev-dir/file
	    (cons (file-name-directory file) (file-name-nondirectory file)))
      (sml-send-string (format sml-use-command file) nil and-go))))

(defun sml-cd (dir)
  "Change the working directory of the inferior ML process.
The default directory of the process buffer is changed to DIR.  If the
variable `sml-cd-command' is non-nil it should be an ML command that will
be executed to change the compiler's working directory\; a trailing
\"\;\\n\" will be added automatically."
  (interactive "DSML Directory: ")
  (let ((dir (expand-file-name dir)))
    (with-current-buffer (sml-proc-buffer)
      (sml-send-string (format sml-cd-command dir) t)
      (setq default-directory dir))
    (setq sml-prev-dir/file (cons dir nil))))

(defun sml-send-string (str &optional print and-go)
  (let ((proc (sml-proc))
	(str (concat str ";\n"))
	(win (get-buffer-window (current-buffer) 'visible)))
    (when win (select-window win))
    (goto-char (point-max))
    (when print (insert str))
    (sml-update-cursor)
    (set-marker (process-mark proc) (point-max))
    (setq compilation-last-buffer (current-buffer))
    (comint-send-string proc str)
    (when and-go (switch-to-sml nil))))

(defun sml-compile (command)
  "Pass a COMMAND to the SML process to compile the current program.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

Interactively, prompts for the command if `compilation-read-command' is
non-nil.  With prefix arg, always prompts."
  (interactive
   (let* ((dir default-directory)
	  (cmd "cd \"."))
     ;; look for files to determine the default command
     (while (and (stringp dir)
		 (dolist (cf sml-compile-commands-alist 1)
		   (when (file-exists-p (expand-file-name (cdr cf) dir))
		     (setq cmd (concat cmd "\"; " (car cf))) (return nil))))
       (let ((newdir (file-name-directory (directory-file-name dir))))
	 (setq dir (unless (equal newdir dir) newdir))
	 (setq cmd (concat cmd "/.."))))
     (setq cmd
	   (cond
	    ((local-variable-p 'sml-compile-command) sml-compile-command)
	    ((string-match "^\\s-*cd\\s-+\"\\.\"\\s-*;\\s-*" cmd)
	     (substring cmd (match-end 0)))
	    ((string-match "^\\s-*cd\\s-+\"\\(\\./\\)" cmd)
	     (replace-match "" t t cmd 1))
	    ((string-match ";" cmd) cmd)
	    (t sml-compile-command)))
     ;; code taken from compile.el
     (if (or compilation-read-command current-prefix-arg)
	 (list (read-from-minibuffer "Compile command: "
				     cmd nil nil '(compile-history . 1)))
       (list cmd))))
     ;; ;; now look for command's file to determine the directory
     ;; (setq dir default-directory)
     ;; (while (and (stringp dir)
     ;; 	    (dolist (cf sml-compile-commands-alist t)
     ;; 	      (when (and (equal cmd (car cf))
     ;; 			 (file-exists-p (expand-file-name (cdr cf) dir)))
     ;; 		(return nil))))
     ;;   (let ((newdir (file-name-directory (directory-file-name dir))))
     ;;     (setq dir (unless (equal newdir dir) newdir))))
     ;; (setq dir (or dir default-directory))
     ;; (list cmd dir)))
  (set (make-local-variable 'sml-compile-command) command)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((dir default-directory))
    (when (string-match "^\\s-*cd\\s-+\"\\([^\"]+\\)\"\\s-*;" command)
      (setq dir (match-string 1 command))
      (setq command (replace-match "" t t command)))
    (setq dir (expand-file-name dir))
    (with-current-buffer (sml-proc-buffer)
      (setq default-directory dir)
      (sml-send-string (concat (format sml-cd-command dir) "; " command) t t))))

;;; PARSING ERROR MESSAGES

;; This should need no modification to support other compilers. 

;; Update the buffer-local error-cursor in proc-buffer to be its
;; current proc mark.

(defvar sml-endof-error-alist nil)

(defun sml-update-cursor ()
  ;; Update buffer local variable.
  (set-marker sml-error-cursor (1- (process-mark (sml-proc))))
  (setq sml-endof-error-alist nil)
  (compilation-forget-errors)
  (if (and (fboundp 'compilation-fake-loc) sml-temp-file)
      (compilation-fake-loc (cdr sml-temp-file) (car sml-temp-file)))
  (if (markerp compilation-parsing-end)
      (set-marker compilation-parsing-end sml-error-cursor)
    (setq compilation-parsing-end sml-error-cursor)))

(defun sml-make-error (f c)
  (let ((err (point-marker))
	(linenum (string-to-number c))
	(filename (list (first f) (second f)))
	(column (string-to-number (match-string (third f)))))
    ;; record the end of error, if any
    (when (fourth f)
      (let ((endlinestr (match-string (fourth f))))
	(when endlinestr
	  (let* ((endline (string-to-number endlinestr))
		 (endcol (string-to-number
			  (or (match-string (fifth f)) "0")))
		 (linediff (- endline linenum)))
	    (push (list err linediff (if (= 0 linediff) (- endcol column) endcol))
		  sml-endof-error-alist)))))
    ;; build the error descriptor
    (if (string= (car sml-temp-file) (first f))
	;; special case for code sent via sml-send-region
	(let ((marker (cdr sml-temp-file)))
	  (with-current-buffer (marker-buffer marker)
	    (goto-char marker)
	    (forward-line (1- linenum))
	    (forward-char (1- column))
	    ;; A pair of markers is the right thing to return, but some
	    ;; code in compile.el doesn't like it (when we reach the end
	    ;; of the errors).  So we could try to avoid it, but we don't
	    ;; because that doesn't work correctly if the current buffer
	    ;; has unsaved modifications.  And it's fixed in Emacs-21.
	    ;; (if buffer-file-name
	    ;; 	(list err buffer-file-name
	    ;; 	      (count-lines (point-min) (point)) (current-column))
	    (cons err (point-marker))))	;; )
      ;; taken from compile.el
      (list err filename linenum column))))

(unless (fboundp 'compilation-fake-loc)
(defadvice compilation-goto-locus (after sml-endof-error activate)
  (let* ((next-error (ad-get-arg 0))
	 (err (car next-error))
	 (pos (cdr next-error))
	 (endof (with-current-buffer (marker-buffer err)
		  (assq err sml-endof-error-alist))))
    (if (not endof) (sml-error-overlay 'undo)
      (with-current-buffer (marker-buffer pos)
	(goto-char pos)
	(let ((linediff (second endof))
	      (coldiff (third endof)))
	  (when (> 0 linediff) (forward-line linediff))
	  (forward-char coldiff))
	(sml-error-overlay nil pos (point))
	(push-mark nil t (not sml-error-overlay))
	(goto-char pos))))))

(defun sml-error-overlay (undo &optional beg end)
  "Move `sml-error-overlay' to the text region in the current buffer.
If the buffer-local variable `sml-error-overlay' is
non-nil it should be an overlay \(or extent, in XEmacs speak\)\; this
function moves the overlay over the current region. If the optional
BUFFER argument is given, move the overlay in that buffer instead of
the current buffer.

Called interactively, the optional prefix argument UNDO indicates that
the overlay should simply be removed: \\[universal-argument] \
\\[sml-error-overlay]."
  (interactive "P")
  (when sml-error-overlay
    (unless (overlayp sml-error-overlay)
      (let ((ol sml-error-overlay))
	(setq sml-error-overlay (make-overlay 0 0))
	(overlay-put sml-error-overlay 'face (if (symbolp ol) ol 'region))))
    (if undo (move-overlay sml-error-overlay 1 1 (current-buffer))
      ;; if active regions, signals mark not active if no region set
      (let ((beg (or beg (region-beginning)))
	    (end (or end (region-end))))
	(move-overlay sml-error-overlay beg end (current-buffer))))))

(provide 'sml-proc)

;;; sml-proc.el ends here
