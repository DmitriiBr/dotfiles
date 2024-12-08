(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(load-file "~/.config/emacs/01-ui.el")
(load-file "~/.config/emacs/02-kbd.el")
(load-file "~/.config/emacs/03-lsp.el")
(load-file "~/.config/emacs/04-ivy.el")
(load-file "~/.config/emacs/05-mood-line.el")

;; Best autopair package ever
(use-package flex-autopair
  :ensure t
  :demand t
  :config
  (flex-autopair-mode 1))
;; Autopair end

(use-package move-text
  :ensure t
  :demand t
  :init
  :config
  (global-set-key (kbd "M-p") 'move-text-up)
  (global-set-key (kbd "M-n") 'move-text-down))

(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

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

;; Misc
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))
(use-package add-node-modules-path
  :ensure t
  :init)
(use-package eslint-fix)

;; Modes
(use-package markdown-mode)
(use-package json-mode)
(use-package js2-mode)

(use-package typescript-mode
  :config
  (add-hook 'typescript-mode-hook (lambda () (typescript-mode 1))))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

(use-package tuareg
  :ensure t
  :demand t
  :mode
  (("\\.ocamlinit\\'" . tuareg-mode))
  (("\\.ml\\'" . tuareg-mode))
  (("\\.mli\\'" . tuareg-mode)))

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
  (add-to-list 'apheleia-mode-alist '(typescript-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(web-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(js2-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(json-mode . prettier))
  (apheleia-global-mode +1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tao-yang))
 '(custom-safe-themes
   '("dc15dbd4b0a00c64610fd4379a89424e0be1b418f09457e0f062cac931e8ca82" "b8bd60a23b9e2f08b0c437231ee84f2dacc70fdc4d5a0fb87229bb9926273fdd" "acfe7ff6aacb9432f124cde4e35d6d2b4bc52916411de73a6ccded9750c9fa97" "c0aa9e26715866404ac854a1023a177742b41a3a6b0fdbfe68d9f5414e24e170" "e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7" default))
 '(package-selected-packages
   '(ivy-prescient flymake tao-theme flex-autopair json-mode js2-mode web-mode tuareg mood-line doom-modeline move-text evil projectile counsel lsp-ui smex lsp-mode helm-ls-git helm-git-grep helm exec-path-from-shell company add-node-modules-path apheleia tree-sitter-langs tree-sitter gruber-darker-theme typescript-mode ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t nil)))
 '(flycheck-error ((t (:underline (:color "Red1" :style line :position 0)))))
 '(flycheck-info ((t (:underline (:color "ForestGreen" :style line :position 0)))))
 '(flycheck-warning ((t (:underline (:color "DarkOrange" :style line :position 0)))))
 '(markdown-code-face ((t (:inherit fixed-pitch))))
 '(mode-line ((t nil)))
 '(simple-modeline-status-modified ((t (:inherit font-lock-variable-name-face)))))
