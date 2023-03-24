;; ========= STANDARD SETUP ========= ;;
(load "~/.emacs.d/myrc.el")

;; clean
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(column-number-mode 1)
(show-paren-mode 1) 
(electric-indent-mode 1) ;; dynamically indent text
(size-indication-mode 1) ;; show file size in modeline
(winner-mode 1) ;; enable window-undo/redo
(toggle-word-wrap 1)
;; (global-visual-line-mode 1)

;; font
;; (defun myrc/font () "Fira Code Retina-18")
;; (defun myrc/font () "JetBrains Mono-18")
(defun myrc/font () "Iosevka-18")
(add-to-list 'default-frame-alist `(font . ,(myrc/font)))
(set-face-attribute 'variable-pitch nil :font (myrc/font) :weight 'regular) ;; required for org-mode

;; remove startup message
(setq inhibit-startup-message t)

;; cleaner ~/.emacs.d
;; moving these lines runs the risk of re-downloading all packages from scratch
(setq user-emacs-directory "~/.cache/emacs")
(setq package-user-dir "~/.cache/emacs/elpa")

;; don't show git information on modeline
(setq vc-display-status 0)

;; breathing room
(setq scroll-margin 10)
(make-variable-buffer-local 'scroll-margin)
(setq scroll-conservatively 100) ;; avoid centering point when reaching scroll-margin
(set-fringe-mode '(0 . 0))

;; line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; set specific splits for Compilation (horizontal) and Help (vertical) windows
(setq display-buffer-alist '(("\\*compilation" (display-buffer-reuse-window display-buffer-at-bottom)
			      (window-height . 13))
			     ("\\*Help" (display-buffer-reuse-window display-buffer-in-direction)
			      (direction . right))
			     ;; split documentation windows below *Help* windows
			     (".*\\.gz" (display-buffer-reuse-window display-buffer-below-selected))))

(add-hook 'help-mode-hook 'visual-line-mode)
(myrc/keychain-refresh-environment)

(advice-add 'evil-yank :around 'myrc/evil-yank-pulse)

;; No need to save since it sticks for the daemon's lifetime
;; Default behaviour is to ask
(setq auth-source-save-behavior nil)
;; ============================ ;;


;; ========= MELPA ========= ;;
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
;; use-package will always download package dependencies for you
;; otherwise, :ensure t would have to be specified in every use-package usage
(setq use-package-always-ensure t)
;; ============================ ;;


;; ========= CLEAN ========= ;;
;; cleaner ~/.emacs.d
(use-package no-littering)
(use-package recentf
  :init (recentf-mode)
  :custom ((recentf-save-file "~/.cache/emacs/var/recentf-save.el")))
;; ============================ ;;


;; ========= GENERAL.EL ========= ;;
(use-package general
  :after evil
  :config
  (general-override-mode 1)
  (general-create-definer myrc/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"))
;; ============================ ;;


;; ========= EVIL ========= ;;
;; Press C-z to enter Emacs mode. Press C-z to go back into Evil mode.
(use-package evil
  :init
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-want-C-u-scroll t)
  (evil-want-C-d-scroll t)
  (evil-want-fine-undo t)
  :config
  (evil-mode 1)
  ;; (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) ;; enter normal mode using C-g while in insert mode
  ;; (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)) ;; use C-h to delete characters (same as backspace)
  ;; (define-key evil-normal-state-map (kbd "C-h") 'evil-delete-backward-char-and-join) ;; use C-h to delete characters while in normal mode
  (define-key evil-insert-state-map (kbd "C-h") 'left-char) ;; use C-h to delete characters while in normal mode
  (define-key evil-insert-state-map (kbd "C-l") 'right-char) ;; use C-h to delete characters while in normal mode

  ;; Use visual line motions even outside of visual-line-mode buffers (i.e. when a long line is wrapped, use j/k to get to the wrapped part of it instead of the next/prev line)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion "ag" 'mark-page)
  (evil-global-set-key 'motion "g=" 'evil-numbers/inc-at-pt)
  (evil-global-set-key 'motion "g-" 'evil-numbers/dec-at-pt))

(general-unbind 'normal "g,")
(use-package evil-nerd-commenter
  :after evil
  :config
  (evil-global-set-key 'motion "g," 'evilnc-comment-operator)
  (evil-global-set-key 'motion "g." 'evilnc-copy-and-comment-operator)
  (evil-global-set-key 'motion "g'" 'evilnc-yank-and-comment-operator))
;; :init (evilnc-default-hotkeys)) ;; this binds ',' which is normal used for evil-repeat-find-char-reverse
;; ============================ ;;


;; ========= EVIL-COLLECTION ========= ;;
;; A collection of Evil bidning for the parts of Emacs that Evil does not cover properly by default (e.g. help-mode calendar, eshell etc.)
;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :after evil
  :custom ((evil-want-Y-yank-to-eol t)) ;; can't set in evil configuration because it's probably altered by evil-collection
  ;; (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))
;; ============================ ;;


;; ========= STANDALONE KEYBINDS ========= ;;
;; use (define-key x-mode-map ...) to define a keybinding for a specific mode (e.g. python-mode/rust-mode)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-<tab>") 'indent-for-tab-command)

(myrc/leader-keys
  "u"  '(universal-argument :which-key "universal-argument")
  ;; TOGGLE
  "t"  '(:ignore t :which-key "toggle")
  "tw" '(toggle-word-wrap :which-key "line wrap")
  "tr" '(read-only-mode :which-key "read-only-mode")
  "tt" '(toggle-truncate-lines :which-key "toggle-truncate-lines")
  "ts" '(tree-sitter-mode :which-key "tree-sitter-mode")

  ;; HELP
  "h"  '(:ignore t :which-key "help")
  "hf" '(describe-function :which-key "describe-function")
  "hc" '(describe-command :which-key "describe-command")
  "hv" '(describe-variable :which-key "describe-variable")
  "hk" '(describe-key :which-key "describe-key")
  "hm" '(describe-mode :which-key "describe-mode")
  "ht" '(consult-theme :which-key "load-theme")
  "hb" '(describe-bindings :which-key "describe-bindings")
  "hp" '(describe-package :which-key "describe-package")

  ;; FILE
  "f"  '(:ignore t :which-key "file")
  "fs" '(save-buffer :which-key "save file")
  "fr" '(consult-recent-file :which-key "recent file")
  "." '(find-file :which-key "find-file")
  "ff" '(find-file :which-key "find-file")

  ;; SUDO
  "s"  '(:ignore t :which-key "sudo")
  "s/" '((lambda () (interactive) (find-file (expand-file-name "/sudo::/"))) :which-key "dired sudoedit /")
  "s~" '((lambda () (interactive) (find-file (expand-file-name "/sudo::/home/neeto"))) :which-key "dired sudoedit ~")
  "s." '((lambda () (interactive) (find-file (expand-file-name (concat "/sudo::" (buffer-file-name))))) :which-key "sudoedit current buffer")

  ;; CONFIG
  "d"  '(:ignore t :which-key "config files")
  "di" '((lambda () (interactive) (find-file (expand-file-name "~/repos/dotfiles/my-emacs/.emacs.d/init.el"))) :which-key "init")
  "dr" '((lambda () (interactive) (find-file (expand-file-name "~/repos/dotfiles/my-emacs/.emacs.d/myrc.el"))) :which-key "myrc")
  "dp" '((lambda () (interactive) (find-file (expand-file-name "~/repos/dotfiles/my-emacs/.emacs.d/project-list.el"))) :which-key "project-list")

  ;; BUFFER
  "b"  '(:ignore t :which-key "buffer")
  ","  '(switch-to-buffer :which-key "switch-to-buffer")
  "<"  '(consult-buffer :which-key "consult-buffer")
  "bk" '(kill-this-buffer :which-key "kill-this-buffer")
  "bl" '(evil-switch-to-windows-last-buffer :which-key "last buffer")
  "b]" '(next-buffer :which-key "next-buffer")
  "b[" '(previous-buffer :which-key "previous-buffer")
  "bi" '(ibuffer :which-key "ibuffer")
  "br" '(evil-edit :which-key "refresh buffer")

  ;; WINDOW
  "w"  '(:ignore t :which-key "window")
  ;; kill
  "wc" '(delete-window :which "delete-window")
  "k"  '(kill-buffer-and-window :which "kill-buffer-and-window")
  "w C-o" '(delete-other-windows :which "delete-other-windows")
  "w o" '(delete-other-windows :which "delete-other-windows")
  ;; movement
  "wj" '(evil-window-down :which-key "evil-window-down")
  "wk" '(evil-window-up :which-key "evil-window-up")
  "wh" '(evil-window-left :which-key "evil-window-left")
  "wl" '(evil-window-right :which-key "evil-window-right")
  "wb" '(evil-window-bottom-right :which-key "evil-window-bottom-right")
  ;; splitting
  "ws" '(evil-window-split :which-key "split window [H]")
  "wS" '(evil/window-split-and-follow :which-key "split and follow [H]")
  "wv" '(evil-window-vsplit :which-key "split window [V]")
  "wV" '(evil/window-vsplit-and-follow :which-key "split and follow [V]")
  ;; adjust size
  "w-" '(evil-window-decrease-height 10 :which-key "decrease window height")
  "w=" '(evil-window-increase-height 10 :which-key "increase window height")
  "w<" '(evil-window-decrease-width 20 :which-key "decrease window width")
  "w>" '(evil-window-increase-width 20 :which-key "increase window width")
  "wB" '(balance-windows :which-key "balance-windows")
  "w C-r" '(evil-window-rotate-upwards :which-key "rorate windows")
  ;; undo-redo
  "wu" '(winner-undo :which-key "winner-undo")
  "wr" '(winner-redo :which-key "winner-redo")

  ;; INFERIOR PROCESSES
  "i"  '(:ignore t :which-key "inferior processes")
  "ip" '(run-python :which-key "python interpreter")
  "ie" '(eshell :which-key "eshell")
  "it" '(term :which-key "term")

  ;; EVAL
  "e"  '(:ignore t :which-key "eval")
  "eb" '(eval-buffer :which-key "eval-buffer")
  "ee" '(eval-expression :which-key "eval-expression")
  "es" '(eval-last-sexp :which-key "eval-last-sexp")

  ;; MISC
  "x"  '(evil-buffer-new :which-key "temp buffer")
  "m"  '(man :which-key "man")
  "/"  '(consult-line :which-key "search")
  "qq" '(evil-save-and-quit :which-key "save and quit"))
;; ============================ ;;


;; ========= COMPLETION FRAMEWORK ========= ;;
(use-package vertico
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous))
  :custom (vertico-cycle t)
  :init (vertico-mode))

(use-package consult
  :defer t
  :bind (("C-f" . consult-line)
	 ("C-M-l" . consult-imenu)
	 :map minibuffer-local-map
	 ("C-r" . consult-hitory))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (consult)
  :config
  (setq consult-ripgrep-args (concat consult-ripgrep-args " --hidden"))
  (consult-preview-at-point-mode))

(use-package savehist
  :after vertico
  :config (savehist-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config (marginalia-mode))

(use-package orderless
  :after vertico
  :config
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

(use-package company
  :init (global-company-mode)
  :custom ((company-selection-wrap-around t))
  :bind (:map evil-insert-state-map
	      ("<tab>" . company-complete)))
;; (use-package corfu
;;   :init (global-corfu-mode)
;;   :bind (:map corfu-map
;; 	      ("C-j" . corfu-next)
;; 	      ("C-k" . corfu-previous)
;; 	      ("C-f" . corfu-insert))
;;   :custom
;;   (corfu-auto t)
;;   (corfu-cycle t))

(use-package embark
  :bind (("C-," . embark-act)
	 :map minibuffer-local-map
	 ("C-d" . embark-act)))

(use-package embark-consult
  :after (embark consult)
  :defer t
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))

(setq embark-indicators
      '(myrc/embark-which-key-indicator
	embark-highlight-indicator
	embark-isearch-highlight-indicator))

(advice-add #'embark-completing-read-prompter
	    :around #'myrc/embark-hide-which-key-indicator)
;; ============================ ;;


;; ========= DOOM-MODELINE ========= ;;
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-icon t)
	   (doom-modeline-major-mode-icon t)
	   (doom-modeline-minor-modes nil)
	   (doom-modeline-height 12)
	   (doom-modeline-github nil)
	   (doom-modeline-project-detection nil)))
;; ============================ ;;


;; ========= TERM.el ========= ;;
(use-package term
  :commands (term))
;; :config
;; (setq explicit-shell-file-name "bash"))
;; ============================ ;;


;; ========= THEMES ========= ;;
;; for more themes:
;;;; https://emacsthemes.com
;;;; https://peach-melpa.org

(use-package doom-themes
  ;; :defer t
  :commands (consult-theme))
;; :init (load-theme 'doom-fairy-floss t))
(use-package gruber-darker-theme
  :commands (consult-theme))

;; wombat
(load-theme 'gruber-darker t) ;; t at the end is needed to avoid a warning message
;; ============================ ;;


;; ========= ALL-THE-ICONS ========= ;;
(use-package all-the-icons
  :if (display-graphic-p))
;; ============================ ;;


;; ========= WHICH-KEY ========= ;;
(use-package which-key
  :defer 0
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))
;; ============================ ;;


;; ;; ========= HELPFUL ========= ;;
;; ;; An alternative to the built-in Emacs help that provides much more contextual information.
;; (use-package helpful
;;   :bind
;;   ;; remap: keep the keybind, but change the function that is called when the keybind is used
;;   ([remap describe-function] . helpful-function)
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-variable] . helpful-variable)
;;   ([remap describe-key] . helpful-key))
;; ;; ============================ ;;


;; ========= ORG-MODE ========= ;;
(defun myrc/org-mode-setup ()
  (set-face-attribute 'variable-pitch nil :font (myrc/font) :weight 'regular)
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))
;; (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . myrc/org-mode-setup)
  :bind (([remap org-insert-heading-respect-content] . org-insert-item)
	 ([remap org-table-copy-down] . org-insert-heading))
  :config
  ;; org-ellipsis " ▾"
  (setq org-hide-emphasis-markers t))
;; ============================ ;;


;; ========= MAGIT ========= ;;
(use-package magit
  :commands (magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(myrc/leader-keys
  "g"  '(:ignore t :which-key "magit")
  "gg" '(magit-status :which-key "magit-status")
  "gp" '(magit-pull-from-upstream :which-key "magit-pull"))
;; ============================ ;;


;; ========= PROJECT.el ========= ;;
(use-package project
  :ensure nil
  :custom ((project-list-file "~/.emacs.d/project-list.el")
	   (project-switch-commands #'project-dired)))

(myrc/leader-keys
  "p"  '(:ignore t :which-key "project")
  "pp" '(project-switch-project "~/dev/rust/genp" :which-key "switch project")
  "ps" '(consult-ripgrep :which-key "search project")
  "pd" '(project-dired :which-key "project-dired")
  "SPC" '(project-find-file :which-key "project-find-file"))

;; Turn off vc
;; (with-eval-after-load 'vc
;;   (remove-hook 'find-file-hook 'vc-find-file-hook)
;;   (remove-hook 'find-file-hook 'vc-refresh-state)
;;   (setq vc-handled-backends nil))
;; (add-hook 'project-find-functions 'myrc/git-project-finder)
;; ============================ ;;


;; ========= PROJECTILE ========= ;;
;; (use-package projectile
;;   :defer t
;;   :diminish projectile-mode ;; don't show the mode in the modeline
;;   :config (projectile-mode)
;;   ;; :custom ((projectile-completion-system 'vertico)) ;; 'ido
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map) ;; don't care
;;   :init
;;   (when (file-directory-p "~/dev")
;;     (setq projectile-project-search-path '("~/dev" "~/dev/rust")))
;;   (setq projectile-switch-project-action #'projectile-dired)) ;; projectile-dired is run when switching projects

;; (myrc/leader-keys
;;   "pa" '(projectile-add-known-project :which-key "add project")
;;   "pd" '(projectile-remove-known-project :which-key "remove known project")
;;   "pr" '(projectile-recentf :which-key "recent files"))

;; (use-package consult-projectile
;;   :after projectile)
;; ============================ ;;


;; ========= DIRED ========= ;;
;; Using use-package to configure dired, but not install it since it's built-in (hence :ensure nil)
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  ;; remap "RET" and "-" to use dired-single bindings
  :bind (([remap dired-find-file] . dired-single-buffer)
	 ([remap dired-up-directory] . dired-single-up-directory))
  :custom ((dired-listing-switches "-ahl -v --group-directories-first"))) ;; Flags used to run "ls"

(use-package dired-single
  :after dired)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config (setq all-the-icons-dired-monochrome nil))

(myrc/leader-keys
  "o"  '(:ignore t :which-key "dired")
  "o-" '(dired-jump :which-key "dired-jump")
  "o~" '((lambda () (interactive) (find-file (expand-file-name "/home/neeto/"))) :which-key "dired ~")
  "o/" '((lambda () (interactive) (find-file (expand-file-name "/"))) :which-key "dired /")
  "oo" '(dired :which-key "dired choose"))
;; ============================ ;;


;; ========= COMPILATION-MODE ========= ;;
(setq compile-command "")
(use-package compilation-mode
  :ensure nil
  :commands (compile)
  :bind (("C-<return>" . compilation-display-error)))

(myrc/leader-keys
  "c"  '(:ignore t :which-key "compile")
  "cc" '(compile :which-key "compile")
  "cC" '(recompile :which-key "recompile"))
;; ============================ ;;


;; ========= TREE-SITTER ========= ;;
(use-package tree-sitter
  :hook ((prog-mode . global-tree-sitter-mode))
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :after tree-sitter)
;; ============================ ;;


;; ========= PROGRAMMING-MODES ========= ;;
(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . prettify-symbols-mode)
	 (prog-mode . electric-pair-mode)))

(use-package rust-mode :hook (rust-mode-hook . (setq indent-tabs-mode nil)))
(use-package python-mode :commands (python-mode))
(use-package yaml-mode :commands (yaml-mode))
;; ============================ ;;


;; ========= IEDIT ========= ;;
(use-package iedit
  :demand t
  :bind (:map iedit-mode-occurrence-keymap
	      ("C-j" . iedit-next-occurrence)
	      ("C-k" . iedit-prev-occurrence)
	      ("C-n" . iedit-expand-down-to-occurrence)
	      ("C-p" . iedit-expand-up-to-occurrence)
	      ("C-r" . iedit-restrict-function)
	      ("C-l" . iedit-restrict-current-line)
	      :map evil-normal-state-map ;; needed so that when attempting to enter normal mode from insert mode it doesn't exit altogether
	      ("<escape>" . iedit--quit)))
;; ============================ ;;


;; ========= MOVE-TEXT ========= ;;
(use-package move-text
  :config
  (define-key evil-insert-state-map (kbd "C-n") 'move-text-down)
  (define-key evil-insert-state-map (kbd "C-p") 'move-text-up)
  (define-key evil-normal-state-map (kbd "C-n") 'move-text-down)
  (define-key evil-normal-state-map (kbd "C-p") 'move-text-up)
  (define-key evil-motion-state-map (kbd "C-n") 'move-text-region-down)
  (define-key evil-motion-state-map (kbd "C-p") 'move-text-region-up))
;; ============================ ;;


;; ========= MISCELLANEOUS ========= ;;
;; Dynamically shows evil-search-{forward,backward} results on modeline
(use-package anzu
  :diminish
  :init (global-anzu-mode))

;; turn on manually when needed
(use-package rainbow-mode
  :diminish
  :commands (rainbow-mode))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; Apply various settings for inferior process buffers (e.g. python interpreter, eshell, term)
(dolist (mode '(term-mode-hook
		eshell-mode-hook
		comint-mode-hook)) ;; general command-interpreter-in-buffer
  (add-hook mode #'myrc/inferior-process-mode))

;; STARTUP HOOKS
(add-hook 'server-mode-hook #'myrc/frame-title)
(add-hook 'emacs-startup-hook #'myrc/frame-title)
(add-hook 'emacs-startup-hook #'myrc/display-startup-time)
;; ============================ ;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(iedit anzu yaml-mode which-key vertico use-package tree-sitter rust-mode rainbow-mode rainbow-delimiters python-mode orderless no-littering marginalia magit helpful gruber-darker-theme general evil-nerd-commenter evil-collection doom-themes doom-modeline dired-single corfu consult-projectile all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )