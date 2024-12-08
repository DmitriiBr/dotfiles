;; Mood line start
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode)
  ;; Custom format:
  (setq mood-line-glyph-alist mood-line-glyphs-fira-code)
  (setq mood-line-format
        (mood-line-defformat
         :left
         (((mood-line-segment-buffer-status) . " ")
          ((mood-line-segment-buffer-name)   . ""))
         :right
         (((mood-line-segment-scroll)             . " ")
          ((mood-line-segment-cursor-position)    . "  ")))))
;; Mood line end

(provide '05-mood-line)

;;; 05-mood-line.el ends here
