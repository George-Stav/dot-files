(defun evil/window-vsplit-and-follow ()
  "Split current window vertically, then focus new window.
If `evil-vsplit-window-right' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-vsplit-window-right (not evil-vsplit-window-right)))
    (call-interactively #'evil-window-vsplit)))

(defun evil/window-split-and-follow ()
  "Split current window horizontally, then focus new window.
If `evil-split-window-below' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-split-window-below (not evil-split-window-below)))
    (call-interactively #'evil-window-split)))

(defun myrc/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-porject-root)))

(defun myrc/inferior-process-mode ()
  (setq scroll-margin 0)
  (display-line-numbers-mode 0))

(defun myrc/display-startup-time ()
  "Display emacs startup time and garbage collection."
  (interactive)
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(defun myrc/keychain-refresh-environment ()
  "Set ssh-agent and gpg-agent environment variables.
Set the environment variables `SSH_AUTH_SOCK', `SSH_AGENT_PID'
and `GPG_AGENT' in Emacs' `process-environment' according to
information retrieved from files created by the keychain script."
  (interactive)
  (let* ((ssh (shell-command-to-string "keychain -q --noask --agents ssh --eval"))
         (gpg (shell-command-to-string "keychain -q --noask --agents gpg --eval")))
    (list (and ssh
               (string-match "SSH_AUTH_SOCK[=\s]\\([^\s;\n]*\\)" ssh)
               (setenv       "SSH_AUTH_SOCK" (match-string 1 ssh)))
          (and ssh
               (string-match "SSH_AGENT_PID[=\s]\\([0-9]*\\)?" ssh)
               (setenv       "SSH_AGENT_PID" (match-string 1 ssh)))
          (and gpg
               (string-match "GPG_AGENT_INFO[=\s]\\([^\s;\n]*\\)" gpg)
               (setenv       "GPG_AGENT_INFO" (match-string 1 gpg))))))

(defun myrc/git-project-finder (dir)
  "Integrate .git project roots."
  (let ((dotgit (and (setq dir (locate-dominating-file dir ".git"))
		     (expand-file-name dir))))
    (and dotgit
	 (cons 'transient (file-name-directory dotgit)))))

;; https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
(defun myrc/embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(defun myrc/embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(defun myrc/desktop-save-autoconfirm (orig-fn &rest args)
  "Advice to autoconfirm when saving desktop state."
  (apply orig-fn args))

(defun myrc/evil-yank-pulse (orig-fn beg end &rest args)
  "Advice to add momentary pulse upon yank."
  (pulse-momentary-highlight-region beg end)
  (apply orig-fn beg end args))

(defun myrc/frame-title ()
  "Set frame title and server name."
  (unless (boundp 'server-name)
    (setq server-name "server"))
  (setq frame-title-format (concat "%b - [" server-name "]")))

(defcustom myrc/compilation-window-kill-on-success-var nil
  "Close compilation window on success."
  :type 'boolean)

(defun myrc/toggle-compilation-window-kill-on-success ()
  "Toggle myrc/compilation-window-kill-on-success."
  (interactive)
  (setq myrc/compilation-window-kill-on-success-var
	(not myrc/compilation-window-kill-on-success-var))
  (message "Set myrc/compilation-window-kill-on-success-var to %s" myrc/compilation-window-kill-on-success-var))

(defun myrc/compilation-window-kill-on-success (buf str)
  "Hook for myrc/compilation-window-kill-on-success.
Needs to contain a `finished' message, as well as have 0 errors, warnings and infos. Can be toggled."
  (interactive)
  (if (and (not (null (string-match ".*finished.*" str)))
	   compilation-num-errors-found
	   compilation-num-infos-found
	   compilation-num-warnings-found
	   myrc/compilation-window-kill-on-success-var)
      ;; no errors; kill compilation window
      (progn (delete-windows-on
	      (get-buffer-create "*compilation*"))
	     (message "Compilation finished successfully"))))

(defun myrc/yank-file-name (full)
  "Place filename in kill-ring. Only filename if FULL is nil, else full path."
  (interactive)
  (let ((filename (if full
		      (buffer-file-name)
		    (f-filename (buffer-file-name)))))
    (kill-new filename)
    (message filename)))

(defun myrc/desktop-save (&optional RELEASE ONLY-IF-CHANGED VERSION)
  "My version of desktop-save that sets DIRNAME automatically."
  (unless (f-exists-p desktop-dirname)
    (make-directory desktop-dirname t))
  (desktop-save desktop-dirname RELEASE ONLY-IF-CHANGED VERSION))

(defun myrc/kill-emacs (&optional FORCE)
  "Gracefully kill emacs after saving buffers. If FORCE is t, then save desktop state as well."
  (interactive)
  (if FORCE
      (progn
	(delete-file (desktop-full-file-name))
	(myrc/desktop-save t)))
  (let ((current-prefix-arg 4)) ;; simulate call with universal prefix (C-u/SPC-u)
    (call-interactively 'save-buffers-kill-emacs)))
