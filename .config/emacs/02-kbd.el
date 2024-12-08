;; Redefine some keyboard shortcuts

;; Kill buffers instantly
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; List buffers in minibuffer without spawning annoying "BuffersList Buffer"
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

;; Switch splits wihtout pain
(global-set-key (kbd "C-x C-o") 'other-window)

;; Compilation mode
(global-set-key (kbd "C-c C-p") 'compile)

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

;; Set ESC key to be always quit
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(provide '02-kbd)

;;; 02-kbd.el ends here
