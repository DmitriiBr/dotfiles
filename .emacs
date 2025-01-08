;;; package --- Summary: DmitriiBr config
;;; Commentary:
;;; Custom Emacs config, everything in .config/emacs/config.org file
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; Loading config from .org file
;; (org-babel-load-file "~/.config/emacs/config.org")
(load-file "~/.config/emacs/dm.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tao-yang))
 '(custom-safe-themes
   '("9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" "2771ec93656faf267521dce9ffe1a6ad88cd0bea87aa0e8c4fc80bf355c58c1d" "e978b5106d203ba61eda3242317feff219f257f6300bd9b952726faf4c5dee7b" "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9" "d6b934330450d9de1112cbb7617eaf929244d192c4ffb1b9e6b63ad574784aad" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d" "bbb13492a15c3258f29c21d251da1e62f1abb8bbd492386a673dcfab474186af" "dc15dbd4b0a00c64610fd4379a89424e0be1b418f09457e0f062cac931e8ca82" "b8bd60a23b9e2f08b0c437231ee84f2dacc70fdc4d5a0fb87229bb9926273fdd" "acfe7ff6aacb9432f124cde4e35d6d2b4bc52916411de73a6ccded9750c9fa97" "c0aa9e26715866404ac854a1023a177742b41a3a6b0fdbfe68d9f5414e24e170" "e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7" default))
 '(package-selected-packages
   '(vertico-multiform consult orderless vertico evil-collection gcmh doom-themes apropospriate-theme utop which-key smartparens general flymake tao-theme json-mode js2-mode web-mode tuareg mood-line move-text evil lsp-ui smex lsp-mode helm-ls-git helm-git-grep helm exec-path-from-shell company add-node-modules-path apheleia tree-sitter-langs tree-sitter gruber-darker-theme typescript-mode)))
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

(provide '.emacs)
;;; .emacs ends here
