;; Enabling evil mode

(use-package evil
  :ensure t
  :init
  :config
  (evil-mode 1))

(setq evil-insert-state-cursor '(bar . 3))

(provide '06-evil)

;;; 06-evil.el ends here

