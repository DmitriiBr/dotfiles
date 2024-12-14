;; Redefine some keyboard shortcuts



;; Switch splits without pain
;; (global-set-key (kbd "C-x C-o") 'other-window)

;; Compilation mode
;; (global-set-key (kbd "C-c C-p") 'compile)

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

;; Duplicate line and move to next
(defun duplicate-line-and-next ()
  (interactive)
  (duplicate-line)
  (next-line))
(global-set-key (kbd "C-,") 'duplicate-line-and-next)

;; Killing buffer instead of window
(evil-ex-define-cmd "q" 'kill-current-buffer)
(evil-ex-define-cmd "wq" (lambda () 
                           (interactive)
                           (save-buffer)
                           (kill-current-buffer)))

(use-package general
  :ensure t)

(general-create-definer leader-def
  :states 'motion
  :keymaps 'override
  :prefix "SPC")

;; Set ESC key to be always quit
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(leader-def
  "k" '(:ignore t :which-key "sexp")
  "k w" '(sp-wrap-round :which-key "wrap ()")
  "k [" '(sp-wrap-square :which-key "wrap []")
  "k {" '(sp-wrap-square :which-key "wrap {}")
  "k ," '(sp-forward-barf-sexp :which-key "<-)")
  "k ." '(sp-forward-slurp-sexp :which-key ")->")
  "k <" '(sp-backward-barf-sexp :which-key "<-(")
  "k >" '(sp-forward-barf-sexp :which-key "(->")
  "k r" '(sp-raise-sexp :which-key "raise sexp"))

(leader-def
  "f" '(:ignore t :which-key "file")
  ;; Improved find file
  "f f" '(counsel-find-file :which-key "find file")
  "f g" '(counsel-git :which-key "counsel git")
  ;; Find all occurences in git repo
  "g g" '(counsel-git-grep :which-key "counsel git grep")
  "f s" '(save-buffer :which-key "save file"))

(leader-def
  :keymaps 'emacs-lisp-mode-map
  "m" '(:ignore t :which-key "emacs lisp")
  "m e" '(:ignore t :which-key "eval")
  "m e e" 'eval-last-sexp)

(leader-def
  ;;"p" 'projectile-command-map
  "p" '(:ignore t :which-key "project")
  "p f" '(projectile-find-file :which-key "find file in project")
  "p d" '(projectile-find-dir :which-key "find dir in project"))

(leader-def
  ;; Improved M-x with counsel
  "SPC" '(counsel-M-x :which-key "M-x")
  "s b" '(switch-to-buffer :which-key "switch to buffer"))

(provide '02-kbd)

;;; 02-kbd.el ends here
