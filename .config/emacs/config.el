(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(desktop-save-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(setq visible-bell nil)
(global-display-line-numbers-mode -1)

(setq inhibit-startup-screen t)
(setq scroll-step 1)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(setq-default truncate-lines t)
(setq-default gloabal-visual-mode t)
(global-auto-revert-mode t)

(global-hl-line-mode 1) (add-to-list 'custom-theme-load-path "~/.emacs.d/etc/themes")

(use-package tao-theme
  :ensure t
  :init
  (setq tao-theme-use-sepia nil)
  (setq tao-theme-use-boxes nil))
(set-face-attribute 'default nil :font "Iosevka NF"  :height 190)

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode)
  (setq mood-line-glyph-alist mood-line-glyphs-fira-code)
  (setq mood-line-format
	(mood-line-defformat
	 :left
	 (((mood-line-segment-buffer-status) . " ")
	  ((mood-line-segment-buffer-name)   . ""))
	 :right
	 (((mood-line-segment-scroll)             . " ")
	  ((mood-line-segment-cursor-position)    . "  ")))))

(use-package evil
  :ensure t
  :init
  :config
  (evil-mode 1))

(setq evil-insert-state-cursor '(bar . 3))
(setq evil-want-C-i-jump nil)

(ido-mode 1)
(ido-everywhere 1)
(setq ido-separator "\n")

(use-package ivy
  :ensure t
  :demand t
  :init
  :config
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (ivy-mode +1)
  (ivy-prescient-mode +1))

(use-package counsel
  :ensure t
  :demand t
  :init
  :config (counsel-mode +1))

(use-package projectile
  :ensure t
  :demand t
  :init
  :config
  (projectile-mode +1))

(setq projectile-completion-system 'ivy)

(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode
	 text-mode
	 markdown-mode
	 tuareg-mode
	 emacs-lisp-mode
	 typescript-mode
	 web-mode
	 js2-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package flymake
  :ensure t
  :init
  :config (flymake-mode t)
  :hook (emacs-lisp-mode typesript-mode))

(use-package lsp-mode
  :ensure t
  :demand t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-log-io nil)
  (setq lsp-diagnostics-provider :flymake)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-text-document-color t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-completion-provider :none)
  :hook (
	 ;; (tuareg-mode . lsp)
	 (typescript-mode . lsp)
	 (web-mode . lsp)
	 (js2-mode))
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
