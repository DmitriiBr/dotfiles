(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; Loading config from .org file
(org-babel-load-file "~/.config/emacs/config.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(spacemacs-light))
 '(custom-safe-themes
   '("bbb13492a15c3258f29c21d251da1e62f1abb8bbd492386a673dcfab474186af" "dc15dbd4b0a00c64610fd4379a89424e0be1b418f09457e0f062cac931e8ca82" "b8bd60a23b9e2f08b0c437231ee84f2dacc70fdc4d5a0fb87229bb9926273fdd" "acfe7ff6aacb9432f124cde4e35d6d2b4bc52916411de73a6ccded9750c9fa97" "c0aa9e26715866404ac854a1023a177742b41a3a6b0fdbfe68d9f5414e24e170" "e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7" default))
 '(package-selected-packages
   '(which-key spacemacs-theme smartparens general ivy-prescient flymake tao-theme json-mode js2-mode web-mode tuareg mood-line doom-modeline move-text evil projectile counsel lsp-ui smex lsp-mode helm-ls-git helm-git-grep helm exec-path-from-shell company add-node-modules-path apheleia tree-sitter-langs tree-sitter gruber-darker-theme typescript-mode ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t nil)))
 '(flycheck-error ((t (:underline (:color "Red1" :style line :position 0)))))
 '(flycheck-info ((t (:underline (:color "ForestGreen" :style line :position 0)))))
 '(flycheck-warning ((t (:underline (:color "DarkOrange" :style line :position 0)))))
 '(markdown-code-face ((t (:inherit fixed-pitch))))
 '(mode-line ((t nil)))
 '(simple-modeline-status-modified ((t (:inherit font-lock-variable-name-face)))))
