;;; lsp.el -*- lexical-binding: t; -*-

;;; Useful link on toggling LSP settings: https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

(defun toggle-lsp-completion-mode ()
  ;; (interactive) required to transform a function into a command
  (interactive)
  (if lsp-completion-mode
      (lsp-completion--disable)
    (lsp-completion--enable)))

(use-package! lsp-mode
  :bind (:map lsp-mode-map
         ("C-c C-c l" . flycheck-list-errors)
         ("C-c C-c RET" . 'toggle-lsp-completion-mode))
  :config
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-lens-enable nil)
  (setq lsp-eldoc-enable-hover t)

  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0))

(use-package! lsp-ui
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-delay 0))
