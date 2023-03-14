;; ========= STANDARD SETUP ========= ;;
;; clean
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; font
(set-face-attribute 'default nil :font "Fira Code Retina" :height 180)

;; remove startup message
(setq inhibit-startup-message t)

;; breathing room
;; (set-fringe-mode 10)

;; line numbers
(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
;;;; disable it for some modes
(dolist (mode '(term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
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


;; ========= GENERAL.EL ========= ;;
(use-package general
  :config
  ;; (general-evil-setup t))
  (general-create-definer myrc/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))
;; ============================ ;;


;; ========= EVIL ========= ;;
;; Modes listed here will start in emacs mode (i.e. evil mode is turned off) when started.
;; Use C-z to get back into evil mode.
(defun myrc/evil-hook ()
  (dolist (mode '(eshell-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  ;; (setq evil-want-C-i-jump nil)
  ;; :hook (evil-mode . myrc/evil-hook)
  :config
  (evil-mode 1)
  ;; (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) ;; enter normal mode using C-g while in insert mode
  ;; (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)) ;; use C-h to delete characters (same as backspace)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-delete-backward-char-and-join) ;; use C-h to delete characters while in normal mode

  ;; Use visual line motions even outside of visual-line-mode buffers (i.e. when a long line is wrapped, use j/k to get to the wrapped part of it instead of the next/prev line)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

  ;; (evil-set-initial-state 'messages-buffer-mode 'normal)
  ;; (evil-set-initial-state 'dashboard-mode 'normal))
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

;; TOGGLE
(myrc/leader-keys
 "t"  '(:ignore t :which-key "toggle")
 "tw" '(toggle-truncate-lines :which-key "line wrap")) ;; check out visual-line-mode to prevent cutting of words

;; HELP
(myrc/leader-keys
 "h"  '(:ignore t :which-key "help")
 "hf" '(describe-function :which-key "describe-function")
 "hv" '(describe-variable :which-key "describe-variable")
 "hk" '(describe-key :which-key "describe-key")
 "ht" '(counsel-load-theme :which-key "load-theme"))

(defun +evil/window-vsplit-and-follow ()
  "Split current window vertically, then focus new window.
If `evil-vsplit-window-right' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-vsplit-window-right (not evil-vsplit-window-right)))
    (call-interactively #'evil-window-vsplit)))

(defun +evil/window-split-and-follow ()
  "Split current window horizontally, then focus new window.
If `evil-split-window-below' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-split-window-below (not evil-split-window-below)))
    (call-interactively #'evil-window-split)))

;; WINDOW
(myrc/leader-keys
 "w"  '(:ignore t :which-key "window")
 "wc" '(delete-window :which "delete-window")
 ;; movement
 "wj" '(evil-window-down :which-key "evil-window-down")
 "wk" '(evil-window-up :which-key "evil-window-up")
 "wh" '(evil-window-left :which-key "evil-window-left")
 "wl" '(evil-window-right :which-key "evil-window-right")
 ;; splitting
 "ws" '(evil-window-split :which-key "evil-window-split")
 "wS" '(+evil/window-split-and-follow :which-key "+evil/window-split-and-follow")
 "wv" '(evil-window-vsplit :which-key "evil-window-vsplit")
 "wV" '(+evil/window-vsplit-and-follow :which-key "+evil/window-vsplit-and-follow"))
;; ============================ ;;


;; ========= IVY ========= ;;
;; A generic completion mechanism for Emacs.
(use-package ivy
  :diminish
  :bind (("C-s" . swiper) ;; Mode specific keybinds
         :map ivy-minibuffer-map ;; Buffer specific keybinds
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^
;; ============================ ;;


;; ========= COUNSEL ========= ;;
;; A collection of Ivy-enhanced versions of common Emacs commands.
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . councel-ibuffer)
         ("C-x C-f" . councel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'councel-minibuffer-history)))
;; ============================ ;;


;; ========= IVY-RICH ========= ;;
;; Shows descriptions of commands as well as the keybinds that run them.
(use-package ivy-rich
  :init (ivy-rich-mode 1))
;; ============================ ;;


;; ========= SWIPER ========= ;;
;; An Ivy-enhanced alternative to Isearch.
;; ============================ ;;


;; ========= DOOM-MODELINE ========= ;;
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))
;; ============================ ;;


;; ========= THEMES ========= ;;
(use-package doom-themes)
  ;; :init (load-theme 'doom-fairy-floss t))

(load-theme 'wombat)

;; for more themes:
;;;; https://emacsthemes.com
;;;; https://peach-melpa.org
;; ;; ============================ ;;


;; ========= ALL-THE-ICONS ========= ;;
(use-package all-the-icons
  :if (display-graphic-p))
;; ============================ ;;

;; ========= RAINBOW-DELIMITERS ========= ;;
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)) ;; prog-mode is the base mode for all programming languages mode
;; ============================ ;;


;; ========= WHICH-KEY ========= ;;
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))
;; ============================ ;;


;; ========= HELPFUL ========= ;;
;; An alternative to the built-in Emacs help that provides much more contextual information.
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ;; remap: keep the keybind, but change the function that is called when the keybind is used
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . counsel-describe-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
;; ============================ ;;


;; ========= MAGIT ========= ;;
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(myrc/leader-keys
 "g"  '(:ignore t :which-key "magit")
 "gg" '(magit-status :which-key "magit-status"))
;; ============================ ;;


;; ========= PROJECTILE ========= ;;
(general-auto-unbind-keys 1)
(use-package projectile
  :diminish projectile-mode ;; don't show the mode in the modeline
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy)) ;; 'ido
  :bind-keymap
  ("C-c p" . projectile-command-map) ;; don't care
  :init
  (when (file-directory-p "~/dev")
    (setq projectile-project-search-path '("~/dev" "~/dev/rust")))
  (setq projectile-switch-project-action #'projectile-dired)) ;; projectile-dired is run when switching projects

;; (myrc/leader-keys
;;  "p" '(:ignore t :which-key "projectile")
;;  "pp" '(projectile-switch-project :which-key "browse projects"))

(use-package counsel-projectile
  :config (counsel-projectile-mode))
;; ============================ ;;
