(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(eval-when-compile (require 'use-package))

(setq inhibit-startup-screen t)

(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

(global-display-line-numbers-mode)
(setq display-line-numbers 'relative)

(set-face-attribute 'default nil :height 150)
(set-face-attribute 'default nil :font "Fira Code" :height 150)

(setq make-backup-files nil)

(global-auto-revert-mode t)

;; Insert new line below current line
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

(ido-mode t)

(require 'ivy)
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(require 'magit)

(require 'git-gutter)(
global-git-gutter-mode t)

(require 'git-gutter-fringe)
(define-fringe-bitmap 'git-gutter-fr:added [#b11100000] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:modified [#b11100000] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:deleted
  [#b10000000
   #b11000000
   #b11100000
   #b11110000] nil nil 'bottom)

(require 'evil)
(evil-mode -1)

(use-package typescript-mode
	:config
	(add-hook 'typescript-mode-hook (lambda () (typescript-mode 1)))
)

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(use-package rust-mode)

(use-package dockerfile-mode)

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets
	:config
	(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(require 'flycheck)
(global-flycheck-mode)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

(require 'flycheck-rust)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(require 'eglot)
(add-hook 'rust-mode-hook 'eglot-ensure)

(require 'yasnippet)
(setq yas-triggers-in-field nil)

(require 'company)
(global-company-mode)

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
	(add-hook 'typescript-mode-hook #'setup-tide-mode)
)
;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)
