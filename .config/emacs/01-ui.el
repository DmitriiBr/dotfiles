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

(provide '01-ui)

;;; 01-ui.el ends here

