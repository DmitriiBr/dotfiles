;; Searching, Ido + ivy + counsel
(ido-mode 1)
(ido-everywhere 1)
(setq ido-separator "\n")

(use-package ivy
  :ensure t
  :demand t
  :init
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :config
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (ivy-mode +1)
  (ivy-prescient-mode +1))

(use-package counsel
  :ensure t
  :demand t
  :init
  :config (counsel-mode +1))

;; Searching, Ido + ivy + counsel End

(provide '04-ivy)

;;; 04-ivy.el ends here
