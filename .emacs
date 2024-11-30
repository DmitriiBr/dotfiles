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

(desktop-save-mode 1)

(column-number-mode 1)
(show-paren-mode 1)

;; Line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode -1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/etc/themes")

(setq make-backup-files -1)
(setq inhibit-startup-screen t)

;; Truncate lines everywhere (Like in average editors)
(setq-default truncate-lines t)
(setq-default gloabal-visual-mode t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-auto-revert-mode t)



;; Setting font
(defun set-font (index)
  ;; Choosing font family into local variable
  (let ((font-from-list
         (nth index '("MesloLGS Nerd Font" "PragmataPro"))))
    ;; Choosing heigth into local variable
    (let ((height-from-list
           (nth index '(160 180))))
      ;; Setting default face-attribute
      (set-face-attribute 'default nil :font font-from-list :height height-from-list))))

(set-font 1)
;; Setting font end



;; Kill buffers instantly
(global-set-key (kbd "C-x k") 'kill-this-buffer)

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
  :init
  :hook (electric-indent-mode nil))

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
 '(custom-enabled-themes '(alabaster))
 '(custom-safe-themes
   '("c0aa9e26715866404ac854a1023a177742b41a3a6b0fdbfe68d9f5414e24e170" "e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7" default))
 '(package-selected-packages
   '(js2-mode web-mode tuareg mood-line doom-modeline move-text evil flycheck-posframe projectile counsel ocaml-ts-mode lsp-ui smex lsp-mode helm-ls-git helm-git-grep helm exec-path-from-shell company flycheck-inline add-node-modules-path apheleia flycheck tree-sitter-langs tree-sitter gruber-darker-theme typescript-mode ivy)))
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
 '(mode-line ((t (:background "#e8e8e8" :foreground "black" :box (:line-width (1 . 8) :color "#e8e8e8" :style flat-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#e8e8e8" :foreground "black" :box (:line-width (1 . 10) :color "#e8e8e8")))))
 '(simple-modeline-status-modified ((t (:inherit font-lock-variable-name-face)))))
