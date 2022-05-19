;;; package --- Summary
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Commentary:

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;;
;; DejaVu Sans Mono
;; SauceCodePro Nerd Font Mono
;; Monaco
;; Jetbrains Mono
(setq doom-font (font-spec :family "Jetbrains Mono" :size 15))
(setq doom-variable-pitch-font (font-spec :family "Jetbrains Mono" :size 15))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-zenburn)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; Macros

(map! :leader
      (:prefix-map ("\\" . "words")
      :desc "Check word spelling" "\\" #'flyspell-word
      :desc "Find word synonym" "s" #'synonym))

(map! :leader
      :desc "pdf-view-mode" "v" #'pdf-view-mode)

(map! :leader
      (:prefix-map ("e" . "eshell")
       :desc "Open eshell" "e" #'eshell
       :desc "Open vterm" "v" #'vterm
       :desc "Run command" "c" #'eshell-command
       :desc "Run python interpreter" "p" #'run-python))

(map! :leader
      :desc "Dired" "D" #'dired)

(map! :leader
      (:prefix-map ("d" . "ediff")
       :desc "Quit ediff session" "q" #'ediff-quit
       :desc "Run ediff on two files" "d" #'ediff
       :desc "Run ediff on three files" "3" #'ediff3))

(map! :leader
      :desc "kill-buffer-and-window" "k" #'kill-buffer-and-window)

(map! :leader
      :desc "Pull from upstream branch" "g p" #'magit-pull-from-upstream)

(map! :leader
      :desc "Man" "m" #'man)

(map! :leader
      :desc "Save Tex file and export to PDF" "l" #'TeX-command-master)

(map! :leader
      :desc "Run shell command" "!" #'shell-command)

(map! :leader
      :desc "Run shell command asynchronously" "&" #'async-shell-command)

(map! :leader
      :desc "Recompile" "c r" #'recompile)
;;; keychain autoload
(defun keychain-refresh-environment ()
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

(provide 'keychain-environment)
(keychain-refresh-environment)

(setq max-lisp-eval-depth 100000)
;; (setq pyvenv-default-virtual-env-name "/home/george/.virtualenvs/")
(setq org-hide-emphasis-markers t)
(setq auth-sources '("~/.authinfo"))
(setq org-log-done t)
(setq TeX-command-force "LaTeX") ;; Skip prompt when pressing C-c C-c and force run LaTeX
(setq vterm-shell "/bin/bash")
(setq compile-command "")

;; Thesaurus
(defun synonym (syn)
  (interactive (list (save-excursion (car (ispell-get-word nil)))))
  (browse-url (concat "https://www.thesaurus.com/browse/" syn)))

(blink-cursor-mode)
(add-hook 'find-file-hook 'recentf-save-list)

;; Change to normal mode with the following keys
(require 'key-chord)(key-chord-mode 1)
(key-chord-define-global "fj" 'evil-normal-state)
(key-chord-define-global "jf" 'evil-normal-state)

(require 'evil-mc)
(evil-mc-mode 1)

(require 'pdf-tools)



;;; config.el ends here
