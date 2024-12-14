;; Redefine some keyboard shortcuts

;; Kill buffers instantly
;; Dont work with evil
;; (global-set-key (kbd "C-x k") 'kill-this-buffer)

;; List buffers in minibuffer without spawning annoying "BuffersList Buffer"
;; (global-set-key (kbd "C-x C-b") 'switch-to-buffer)

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
  (lambda ()
    (interactive)
    (duplicate-line)
    (next-line)))
(global-set-key (kbd "C-,") (duplicate-line-and-next))

;; Killing buffer instead of window
(global-set-key [remap evil-quit] 'kill-buffer-and-window)
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
  "f" '(:ignore t :which-key "file")
  "f f" '(counsel-find-file :which-key "find file")
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
  "SPC" '(execute-extended-command :which-key "M-x"))

(provide '02-kbd)

;;; 02-kbd.el ends here
