(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode
         text-mode
         markdown-mode
         tuareg-mode
         emacs-lisp-mode
         typescript-mode
         web-mode
         js2-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(provide '08-smartparens)

;;; 08-smartparens.el ends here
