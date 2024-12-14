;; Removing some UI elements from Emacs

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(desktop-save-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(setq visible-bell nil)

;; Line numbers
(global-display-line-numbers-mode -1)
(setq display-line-numbers-mode 'relative)

(add-to-list 'custom-theme-load-path "~/.emacs.d/etc/themes")

;; Making no backup files
(setq scroll-step 1)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Making Cursor line everywhere for now
(global-hl-line-mode 1)

;; Truncate lines everywhere (Like in average editors)
(setq-default truncate-lines t)
(setq-default gloabal-visual-mode t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(global-auto-revert-mode t)

(use-package tao-theme
  :ensure t
  :init
  (setq tao-theme-use-sepia nil)
  (setq tao-theme-use-boxes nil))

;; Setting font
(set-face-attribute 'default nil :font "Iosevka NF"  :height 190)

(provide '01-ui)

;;; 01-ui.el ends here

