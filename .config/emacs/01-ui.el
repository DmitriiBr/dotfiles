;; Removing some UI elements from Emacs
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

(setq make-backup-files nil)
(setq inhibit-startup-screen t)
