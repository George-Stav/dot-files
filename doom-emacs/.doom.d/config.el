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
;; Iosevka
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
(setq doom-theme 'doom-snazzy)
;; nice themes
;; - wombat
;; - doom-zenburn
;; - doom-gruvbox
;; - doom-ephemeral
;; - doom-snazzy

;; PERSONALISED FEATURES

;; Turn off global-hl-line-mode
(defun turn-off-global-hl-line ()
        (remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)
        (setq global-hl-line-mode 'nil))

(if (string= doom-theme "wombat")
    (turn-off-global-hl-line))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; hopefully sto the outdated-byte-compiled file error
(setq load-prefer-newer t)

(setq max-lisp-eval-depth 100000)
(setq org-hide-emphasis-markers t) ;; hide *..* /../ in org-mode
(setq auth-sources '("~/.authinfo"))
(setq org-log-done t)
(setq TeX-command-force "LaTeX") ;; Skip prompt when pressing C-c C-c and force run LaTeX
(setq compile-command "") ;; manually empty the SPC-c-c command
(blink-cursor-mode)
(add-hook 'find-file-hook 'recentf-save-list)
;; (setq vterm-shell "/bin/bash")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys

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

(load! "ext/keymap.el")
(load! "ext/keychain.el")
(load! "ext/lsp.el")

(use-package! key-chord
    :config
    (key-chord-mode t)
    (key-chord-define-global "fj" 'evil-normal-state)
    (key-chord-define-global "jf" 'evil-normal-state))

(use-package! evil-mc-extras
    :config
    (global-evil-mc-mode 1))

;; (use-package! dired-toggle-sudo)
;; (require 'dired-toggle-sudo)

;;; config.el ends here
