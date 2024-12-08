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

(setq make-backup-files nil)
(setq inhibit-startup-screen t)

;; Making Cursor line everywhere for now
(global-hl-line-mode 1)

;; Truncate lines everywhere (Like in average editors)
(setq-default truncate-lines t)
(setq-default gloabal-visual-mode t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(global-auto-revert-mode t)

;; Setting font
(set-face-attribute 'default nil :font "Iosevka NF"  :height 190)

(provide '01-ui)

;;; 01-ui.el ends here

