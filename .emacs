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

;; Line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers 'relative)

(setq make-backup-files nil)
(setq inhibit-startup-screen t)

(global-auto-revert-mode t)

(set-face-attribute 'default nil :height 160)
(set-face-attribute 'default nil :font "MesloLGS Nerd Font" :height 160)

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

(ido-mode 1)
(setq ido-separator "\n")
(ido-everywhere 1)

(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

(use-package ivy
  :ensure t
  :demand t
  :init
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode +1))

(use-package counsel
  :ensure t
  :demand t
  :init
  :config
  (counsel-mode +1))

;; Consel for searching with ivy
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; Finding file in git repo
(global-set-key (kbd "C-c g") 'counsel-git)
;; Finding Occurance in git repo
(global-set-key (kbd "C-c j") 'counsel-git-grep)

(use-package magit
  :ensure t
  :init)

(use-package projectile
  :ensure t
  :demand t
  :init
  :config
  (projectile-mode +1))

(setq projectile-completion-system 'ivy)

;; Keymaps for projectile
(global-set-key (kbd "C-x p f") 'projectile-find-file)
(global-set-key (kbd "C-x p d") 'projectile-find-dir)


(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package add-node-modules-path
  :ensure t
  :init)

(use-package tree-sitter
  :ensure t
  :config
  (use-package tree-sitter-langs
    :ensure t
    :config
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))

;; Setting indentation for ts, js, tsx, js, ocaml to 4
(setq typescript-ts-mode-indent-offset 4)
(setq tsx-ts-mode-indent-offset 4)
(setq ocaml-ts-mode-indent-offset 4)

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
	 ("\\.ml\\'" . ocaml-ts-mode)
	 ("\\.mli\\'" . ocaml-ts-mode)))

;; You need this mappings to add language-grammars to treesit list and install them
;; You can call list for installation using M-x treesit-install-language-grammar
(setq treesit-language-source-alist
      '(
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "grammars/ocaml/src")))

(use-package eslint-rc
  :ensure t
  :init)

;;; APHELEIA
;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/aphelei
(use-package apheleia
  :ensure t
  :demand t
  :config
  ;; You always should get prettier from formatters list and call prettiern bin to format buffer
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier" "--stdin-filepath" filepath))
  ;; Here prettier is connecting to modes
  (add-to-list 'apheleia-mode-alist '(tsx-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(js-ts-mode . prettier))
  (apheleia-global-mode +1))

(use-package flycheck
  :ensure t
  :demand t
  :config
  (global-flycheck-mode)
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)))

;; Setting up an LSP mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-log-io nil)
  (setq lsp-headerline-breadcrumb-enable nil)  ; Optional, I like the breadcrumbs
  :hook
  (tsx-ts-mode . lsp)
  (typescript-ts-mode . lsp)
  (js-ts-mode . lsp)
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(setq doom-modeline-hud nil)
(setq doom-modeline-buffer-encoding nil)
(setq nerd-icons-scale-factor 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7" default))
 '(package-selected-packages
   '(projectile doom-modeline counsel ocaml-ts-mode lsp-ui smex lsp-mode helm-ls-git helm-git-grep helm exec-path-from-shell company flycheck-inline add-node-modules-path apheleia eslint-rc flycheck tree-sitter-langs tree-sitter gruber-darker-theme typescript-mode ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:underline (:color "Red1" :style line :position 0)))))
 '(flycheck-info ((t (:underline (:color "ForestGreen" :style line :position 0)))))
 '(flycheck-warning ((t (:underline (:color "DarkOrange" :style line :position 0))))))
