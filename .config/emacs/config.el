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

(use-package evil
  :ensure t
  :init
  :config
  (evil-mode 1))

(setq evil-insert-state-cursor '(bar . 3))
