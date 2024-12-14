(use-package projectile
  :ensure t
  :demand t
  :init
  :config
  (projectile-mode +1))

(setq projectile-completion-system 'ivy)

;; Keymaps for projectile
;; (global-set-key (kbd "C-x p f") 'projectile-find-file)
;; (global-set-key (kbd "C-x p d") 'projectile-find-dir)

