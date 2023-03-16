;; ========= STANDARD SETUP ========= ;;
(load "~/.emacs.d/myrc.el")

;; clean
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(column-number-mode 1)
(show-paren-mode 1) 
(recentf-mode 1) ;; enable history
(electric-indent-mode 1) ;; dynamically indent text
(size-indication-mode 1) ;; show file size in modeline
(winner-mode 1) ;; enable window-undo/redo

;; font
;; "Fira Code Retina"
(set-face-attribute 'default nil :font "Iosevka" :height 180)

;; remove startup message
(setq inhibit-startup-message t)

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

;; needed to toggle wrap (SPC-t-w) to work properly
;; (toggle-truncate-lines)

;; cleaner ~/.emacs.d
(setq user-emacs-directory "~/.cache/emacs")
(setq package-user-dir "~/.cache/emacs/elpa")

;; set specific splits for Compilation (horizontal) and Help (vertical) windows
;; as well as their dimensions
(setq display-buffer-alist '(("\\*compilation" (display-buffer-reuse-window display-buffer-at-bottom)
			      (window-height . 13))
			     ("\\*help" (display-buffer-reuse-window display-buffer-in-side-window)
			      (side . right)
			      (window-width . 80))))
;; ============================ ;;


;; ========= MELPA ========= ;;
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
;; use-package will always download package dependencies for you
;; otherwise, :ensure t would have to be included in every use-package usage
(setq use-package-always-ensure t)
;; ============================ ;;

;; cleaner ~/.emacs.d
(use-package no-littering)

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
  :config
  (evil-mode 1)
  ;; (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) ;; enter normal mode using C-g while in insert mode
  ;; (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)) ;; use C-h to delete characters (same as backspace)
  ;; (define-key evil-normal-state-map (kbd "C-h") 'evil-delete-backward-char-and-join) ;; use C-h to delete characters while in normal mode

  ;; Use visual line motions even outside of visual-line-mode buffers (i.e. when a long line is wrapped, use j/k to get to the wrapped part of it instead of the next/prev line)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

  ;; (evil-set-initial-state 'messages-buffer-mode 'normal)
  ;; (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-nerd-commenter
  :after evil
  :config
  (evil-global-set-key 'motion "gc" 'evilnc-comment-operator))

;; (use-package evil-iedit-state)
;; ============================ ;;


;; ========= EVIL-COLLECTION ========= ;;
;; A collection of Evil bidning for the parts of Emacs that Evil does not cover properly by default (e.g. help-mode calendar, eshell etc.)
;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
;; ============================ ;;


;; ========= STANDALONE KEYBINDS ========= ;;
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
;; use (define-key x-mode-map ...) to define a keybinding for a specific mode (e.g. python-mode/rust-mode)
;; (define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme)

(myrc/leader-keys
  "u"  '(universal-argument :which-key "universal-argument")
  ;; TOGGLE
  "t"  '(:ignore t :which-key "toggle")
  "tw" '(visual-line-mode :which-key "line wrap")
  "tr" '(read-only-mode :which-key "read-only-mode")

  ;; HELP
  "h"  '(:ignore t :which-key "help")
  "hf" '(describe-function :which-key "describe-function")
  "hc" '(describe-command :which-key "describe-command")
  "hv" '(describe-variable :which-key "describe-variable")
  "hk" '(describe-key :which-key "describe-key")
  "hm" '(describe-mode :which-key "describe-mode")
  "ht" '(consult-theme :which-key "load-theme")
  "hb" '(describe-bindings :which-key "describe-bindings")

  ;; FILE
  "f"  '(:ignore t :which-key "file")
  "fs" '(save-buffer :which-key "save file")
  "fr" '(consult-recent-file :which-key "recent file")
  "SPC" '(find-file :which-key "find-file")
  "ff" '(find-file :which-key "find-file")
  "fi" '((lambda () (interactive) (find-file (expand-file-name "~/repos/dotfiles/my-emacs/.emacs.d/init.el"))) :which-key "open init")

  ;; BUFFER
  "b"  '(:ignore t :which-key "buffer")
  ","  '(switch-to-buffer :which-key "switch-workspace-buffer")
  "<"  '(consult-buffer :which-key "switch-buffer")
  "bk" '(kill-this-buffer :which-key "kill-this-buffer")
  "bl" '(evil-switch-to-windows-last-buffer :which-key "last buffer")
  "b]" '(next-buffer :which-key "next-buffer")
  "b[" '(previous-buffer :which-key "previous-buffer")
  "bi" '(ibuffer :which-key "ibuffer")

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
  "wB" '(balance-windows :which-key "balance-windows")
  "wR" '(evil-window-rotate-upwards :which-key "rorate windows")
  ;; undo-redo
  "wu" '(winner-undo :which-key "winner-undo")
  "wr" '(winner-redo :which-key "winner-redo")

  ;; INFERIOR PROCESSES
  "i"  '(:ignore t :which-key "inferior processes")
  "ip" '(run-python :which-key "python interpreter")
  "ie" '(eshell :which-key "eshell")
  "iv" '(vterm :which-key "vterm")

  ;; EVAL
  "e"  '(:ignore t :which-key "eval")
  "eb" '(eval-buffer :which-key "eval-buffer")
  "ee" '(eval-expression :which-key "eval-expression")
  "es" '(eval-last-sexp :which-key "eval-last-sexp")

  ;; MISC
  "/"  '(consult-line :which-key "search"))
;; ============================ ;;


;; ========= COMPLETION FRAMEWORK ========= ;;
(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :custom (vertico-cycle t)
  :init (vertico-mode))

(use-package consult
  :after vertico
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         :map minibuffer-local-map
         ("C-r" . consult-hitory))
  :custom
  (consult-project-root-function #' myrc/get-project-root)
  (completion-in-region-function #'consult-completion-in-region)
  :config
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

(use-package corfu
  :init (global-corfu-mode)
  :bind (:map corfu-map
	      ("C-j" . corfu-next)
	      ("C-k" . corfu-previous)
	      ("C-f" . corfu-insert))
  :custom
  (corfu-auto t)
  (corfu-cycle t))
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


;; ========= THEMES ========= ;;
(use-package doom-themes)
  ;; :init (load-theme 'doom-fairy-floss t))
(use-package gruber-darker-theme)

(load-theme 'wombat t) ;; t at the end is needed to avoid a warning message

;; for more themes:
;;;; https://emacsthemes.com
;;;; https://peach-melpa.org
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


;; ========= MAGIT ========= ;;
(use-package magit
  ;; :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(myrc/leader-keys
 "g"  '(:ignore t :which-key "magit")
 "gg" '(magit-status :which-key "magit-status")
 "gp" '(magit-pull-from-upstream :which-key "magit-pull"))
;; ============================ ;;


;; ========= PROJECTILE ========= ;;
(use-package projectile
  :defer t
  :diminish projectile-mode ;; don't show the mode in the modeline
  :config (projectile-mode)
  ;; :custom ((projectile-completion-system 'vertico)) ;; 'ido
  :bind-keymap
  ("C-c p" . projectile-command-map) ;; don't care
  :init
  (when (file-directory-p "~/dev")
    (setq projectile-project-search-path '("~/dev" "~/dev/rust")))
  (setq projectile-switch-project-action #'projectile-dired)) ;; projectile-dired is run when switching projects

(myrc/leader-keys
 "p"  '(:ignore t :which-key "projectile")
 "pp" '(projectile-switch-project :which-key "switch project")
 "pa" '(projectile-add-known-project :which-key "add project")
 "ps" '(consult-ripgrep :which-key "search project")
 "pd" '(projectile-remove-known-project :which-key "remove known project")
 "pr" '(projectile-recentf :which-key "recent files")
 "." '(projectile-find-file :which-key "find project file"))

(use-package consult-projectile
  :after projectile)
;; (use-package counsel-projectile
;;   :config (counsel-projectile-mode))
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
 "o-" '(dired-jump :which-key "dired-jump"))
;; ============================ ;;


;; ========= COMPILATION-MODE ========= ;;
(setq compile-command "")
(use-package compilation-mode
  :ensure nil
  :commands (compilation-mode)
  :bind (("C-<return>" . compilation-display-error)))

(myrc/leader-keys
 "c"  '(:ignore t :which-key "compile")
 "cc" '(compile :which-key "compile")
 "cC" '(recompile :which-key "recompile"))
;; ============================ ;;


;; ========= TREE-SITTER ========= ;;
;; (use-package tree-sitter-langs)
(use-package tree-sitter
  :hook ((prog-mode . global-tree-sitter-mode))
  :config
  ;; (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
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


;; ========= MISCELLANEOUS ========= ;;
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
;; ============================ ;;


;; ========= STARTUP ========= ;;
(add-hook 'emacs-startup-hook #'myrc/display-startup-time)
;; ============================ ;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-delimiters rainbow-mode yaml-mode python-mode rust-mode tree-sitter all-the-icons-dired dired-single consult-projectile projectile magit which-key all-the-icons gruber-darker-theme doom-themes doom-modeline corfu orderless marginalia consult vertico evil-collection evil-nerd-commenter evil general no-littering use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
