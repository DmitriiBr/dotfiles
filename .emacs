(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(load-file "~/.config/emacs/01-ui.el")
(load-file "~/.config/emacs/02-kbd.el")

;; Truncate lines everywhere (Like in average editors)
(setq-default truncate-lines t)
(setq-default gloabal-visual-mode t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-auto-revert-mode t)

;; Setting font
(set-face-attribute 'default nil :font "Iosevka NF"  :height 190)
;; Setting font end

;; Kill buffers instantly
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; List buffers in minibuffer without spawning annoying "BuffersList Buffer"
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

;; Switch splits wihtout pain
(global-set-key (kbd "C-x C-o") 'other-window)

;; Compilation mode
(global-set-key (kbd "C-c C-p") 'compile)

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

;; Searching, Ido + ivy + counsel
(ido-mode 1)
(ido-everywhere 1)
(setq ido-separator "\n")

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

;; Searching, Ido + ivy + counsel End

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

(use-package typescript-mode
  :config
  (add-hook 'typescript-mode-hook (lambda () (typescript-mode 1))))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

(use-package js2-mode)

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
  (add-to-list 'apheleia-mode-alist '(tsx-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(js-ts-mode . prettier))
  (apheleia-global-mode +1))

;; Mood line start
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode)
  ;; Custom format:
  (setq mood-line-glyph-alist mood-line-glyphs-fira-code)
  (setq mood-line-format
        (mood-line-defformat
         :left
         (((mood-line-segment-buffer-status) . " ")
          ((mood-line-segment-buffer-name)   . ""))
         :right
         (((mood-line-segment-scroll)             . " ")
          ((mood-line-segment-cursor-position)    . "  ")))))
;; Mood line end

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tao-yang))
 '(custom-safe-themes
   '("dc15dbd4b0a00c64610fd4379a89424e0be1b418f09457e0f062cac931e8ca82" "b8bd60a23b9e2f08b0c437231ee84f2dacc70fdc4d5a0fb87229bb9926273fdd" "acfe7ff6aacb9432f124cde4e35d6d2b4bc52916411de73a6ccded9750c9fa97" "c0aa9e26715866404ac854a1023a177742b41a3a6b0fdbfe68d9f5414e24e170" "e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7" default))
 '(package-selected-packages
   '(tao-theme flex-autopair json-mode js2-mode web-mode tuareg mood-line doom-modeline move-text evil flycheck-posframe projectile counsel lsp-ui smex lsp-mode helm-ls-git helm-git-grep helm exec-path-from-shell company flycheck-inline add-node-modules-path apheleia flycheck tree-sitter-langs tree-sitter gruber-darker-theme typescript-mode ivy)))
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
