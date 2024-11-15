(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(eval-when-compile (require 'use-package))

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)

(global-display-line-numbers-mode)
(setq display-line-numbers 'relative)

(setq make-backup-files nil)

(global-auto-revert-mode t)

(set-face-attribute 'default nil :height 160)
(set-face-attribute 'default nil :font "MesloLGS Nerd Font" :height 160)

(ido-mode t)
(setq ido-separator "\n")

;;' Insert new line below current line
(global-set-key (kbd "<C-return>") (lambda ()
				     (interactive)
				     (end-of-line)
				     (newline-and-indent)))
;; Insert new line above current line

(global-set-key (kbd "<C-S-return>") (lambda ()
				       (interactive)
				       (previous-line)
				       (end-of-line)
				       (newline-and-indent)))


(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package ivy
  :ensure t
  :diminish
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode))


(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))


(use-package tree-sitter
  :ensure t
  :diminish
  :config
  (use-package tree-sitter-langs
    :ensure t
    :diminish
    :config
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1)
  (add-hook 'typescript-mode-hook 'prettier-rc-mode)
  (add-hook 'js2-mode-hook 'prettier-rc-mode)
  (add-hook 'web-mode-hook 'prettier-rc-mode))

(use-package add-node-modules-path)

(use-package eslint-rc
  :ensure t
  :config
  (add-hook 'typescript-mode-hook 'eslint-rc-mode)
  (add-hook 'js2-mode-hook 'eslint-rc-mode)
  (add-hook 'web-mode-hook 'eslint-rc-mode))

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :config
  (global-flycheck-mode)
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)))



(use-package tide
  :init 
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))
  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;; if you use typescript-mode
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; if you use treesitter based typescript-ts-mode (emacs 29+)
(add-hook 'typescript-ts-mode-hook #'setup-tide-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7" default))
 '(package-selected-packages
   '(exec-path-from-shell company tide flycheck-inline add-node-modules-path apheleia eslint-rc flycheck js2-mode web-mode jtsx tree-sitter-langs tree-sitter gruber-darker-theme typescript-mode ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
