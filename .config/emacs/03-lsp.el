;;; package --- Summary 03-lsp.el starts here
;;; Commentary:

;; Im NOT using this in main config]
;; Not using LSP at all

;; LSP MODE IS ENABLED!!!
;; Lsp start
(defun add-lsp-hooks (lsp)
  "Add lsp-mode for this major modes."
  (tuareg-mode . lsp))

(use-package flymake
  :ensure t
  :init
  :config (flymake-mode t))

;; setting up an LSP mode
(use-package lsp-mode
  :ensure t
  :demand t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-log-io nil)
  (setq lsp-diagnostics-provider :flymake)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-headerline-breadcrumb-enable nil)  ; Optional, I like the breadcrumbs
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-text-document-color t)
  (setq lsp-headerline-breadcrumb-enable nil)  ; Optional, I like the breadcrumbs
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-signature-auto-activate nil) ;; you could manually request them via `lsp-signature-activate`
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-completion-provider :none)
  :hook (
         (tuareg-mode . lsp)
         (typescript-mode . lsp)
         (web-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-max-width 70)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  :commands lsp-ui-mode)
;; Lsp end

;; Flycheck start
;; (use-package flycheck
;;:config
;; Making delay to stop showing erorrs on point
;;(setq flycheck-display-errors-delay 999999)
;;(setq flycheck-auto-display-errors-after-checking nil)

;; Disabling flycheck, because using default flymake
;; (global-flycheck-mode)
;;(with-eval-after-load 'flycheck
;;'(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)))

;; Flycheck end

(provide '03-lsp)
;;; 03-lsp.el ends here
