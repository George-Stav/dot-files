;;; package --- Summary
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Commentary:

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;;; Code:
(setq user-full-name "George Stavropoulos"
      user-mail-address "georgestavropoulos0@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;

;; DejaVu Sans Mono
;; SauceCodePro Nerd Font Mono
;; Monaco
;; Jetbrains Mono
(setq doom-font (font-spec :family "Jetbrains Mono" :size 18))
      ;; doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'doom-nord)

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

;; Thesaurus
(defun synonym (syn)
  (interactive (list (save-excursion (car (ispell-get-word nil)))))
  (browse-url (concat "https://www.thesaurus.com/browse/" syn)))

;; Forge
(use-package forge
  :after magit)

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

;; (unless (package-installed-p 'polymode)
;;   (package-install 'poly-markdown))

(use-package pdf-tools)

;; LaTeX
;; Skip prompt when pressing C-c C-c and force run LaTeX
(setq TeX-command-force "LaTeX")

(map! :leader
      :desc "Save Tex file and export to PDF" "l" #'TeX-command-master)

;; (defun markdown-html (buffer)
;;   (princ (with-current-buffer buffer
;;     (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
;;   (current-buffer)))

;; (load! "~/dev/elisp/run-cmd-on-save.el")

(setq auth-sources '("~/.authinfo"))

;; ssh-agent
;; (require 'exec-path-from-shell)
;; (exec-path-from-shell-copy-env "SSH_AGENT_PID")
;; (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

(blink-cursor-mode)
(add-hook 'find-file-hook 'recentf-save-list)

;; org-mode

;; hide [*,/,etc.]
(setq org-hide-emphasis-markers t)

;;; keychain
;;; autoload
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

;; (provide 'config)
;;; config.el ends here
