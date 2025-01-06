(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(desktop-save-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(setq visible-bell nil)
(global-display-line-numbers-mode -1)

(setq inhibit-startup-screen t)
(setq scroll-step 1)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(setq-default truncate-lines t)
(setq-default gloabal-visual-mode t)
(global-auto-revert-mode t)

(global-hl-line-mode 1) (add-to-list 'custom-theme-load-path "~/.emacs.d/etc/themes")
(set-face-attribute 'default nil :font "Iosevka NF"  :height 190)

(use-package tao-theme
  :ensure t
  :init
  :config
  (setq tao-theme-use-boxes nil))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-italic nil)
  (setq doom-themes-enable-bold t)
  (load-theme 'doom-one t)
  (doom-themes-org-config))

;; Using garbage magic hack.
(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode)
  (setq mood-line-glyph-alist mood-line-glyphs-fira-code)
  (setq mood-line-format
	(mood-line-defformat
	 :left
	 (((mood-line-segment-buffer-status) . " ")
	  ((mood-line-segment-buffer-name)   . ""))
	 :right
	 (((mood-line-segment-scroll)             . " ")
	  ((mood-line-segment-cursor-position)    . "  ")))))

(use-package evil
  :ensure t
  :init
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(setq evil-insert-state-cursor '(bar . 3))
(setq evil-want-C-i-jump nil)
(setq evil-move-beyond-eol nil)

(ido-mode 1)
(ido-everywhere 1)
(setq ido-separator "\n")

(use-package ivy
  :ensure t
  :demand t
  :init
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

(use-package which-key
  :ensure t
  :demand t
  :init
  :config
  (which-key-mode +1))

(setq which-key-idle-delay 0.5)
(setq which-key-idle-secondary-delay 0)

(use-package projectile
  :ensure t
  :demand t
  :init
  :config
  (projectile-mode +1))

(setq projectile-completion-system 'ivy)

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

(use-package magit
  :ensure t
  :init)

(use-package markdown-mode)
  (use-package json-mode)



  (use-package typescript-mode
    :config
    (add-hook 'typescript-mode-hook (lambda () (typescript-mode 1))))

  (use-package js2-mode)
  (use-package web-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

(setq org-startup-indented t)

(use-package tuareg
  :ensure t
  :demand t
  :mode
  (("\\.ocamlinit\\'" . tuareg-mode))
  (("\\.ml\\'" . tuareg-mode))
  (("\\.mli\\'" . tuareg-mode)))

(use-package utop
  :ensure t)

(use-package apheleia
  :ensure t
  :demand t
  :config
  ;; You always should get prettier from formatters list and call prettiern bin to format buffer
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier" "--stdin-filepath" filepath))
  ;; Here prettier is connecting to modes
  (add-to-list 'apheleia-mode-alist '(typescript-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(web-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(js2-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(json-mode . prettier))
  (apheleia-global-mode +1))

(use-package flymake
  :ensure t
  :init
  :config (flymake-mode t)
  :hook (emacs-lisp-mode typesript-mode))

(use-package lsp-mode
  :ensure t
  :demand t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-log-io nil)
  (setq lsp-diagnostics-provider :flymake)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-text-document-color t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-completion-provider :none)
  :hook (
	 ;; (tuareg-mode . lsp)
	 (typescript-mode . lsp)
	 (web-mode . lsp)
	 (js2-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-max-width 70)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  :commands lsp-ui-mode)

;; Flycheck start
;; (use-package flycheck
;;:config
;; Making delay to stop showing erorrs on point
;;(setq flycheck-display-errors-delay 999999)
;;(setq flycheck-auto-display-errors-after-checking nil)

;; Disabling flycheck, because using default flymake
;; (global-flycheck-mode)
;;(with-eval-after-load 'flycheck
;;'(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)))

;; Insert new line below current line
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
(global-set-key (kbd "C-,") (lambda ()
                              (interactive)
                              (duplicate-line)
                              (next-line)))

(evil-ex-define-cmd "q" 'kill-current-buffer)
(evil-ex-define-cmd "wq" (lambda () 
			   (interactive)
			   (save-buffer)
			   (kill-current-buffer)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :ensure t)

(general-create-definer leader-def
  :states '(normal insert motion visual emacs)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

(leader-def
  ;; Improved M-x with counsel
  "SPC" '(counsel-M-x :which-key "M-x")
  "w s m" '(which-key-show-major-mode :which-key "[W]K [M]AJOR MODE KEYMAPS"))

(general-define-key
 :states '(visual normal motion)
 :keymaps 'override
 "$" 'evil-last-non-blank
 "-" 'evil-last-non-blank)

(leader-def
  "b" '(:ignore t :which-key "[B]uffer")
  "b s" '(switch-to-buffer :which-key "[S]witch to buffer")
  "b p" '(previous-buffer :which-key "[P]revious buffer")
  "b n" '(next-buffer :which-key "[N]ext buffer"))

(defun dm/org-insert-name (&optional count)
  "Inserting a `#+NAME:` tag, and then, entering `-- INSERT --` state,
   good for creating named src's"
  (interactive)
  (insert "#+NAME: ")
  (evil-insert count))

(general-define-key
 :states 'motion
 :keymaps 'org-mode-map
 "<tab>" 'org-cycle
 "g <tab>" 'org-cycle-global)

(leader-def
  :keymaps 'org-mode-map
  "m" '(:ignore t :which-key "[M]AJOR MODE KEYMAPS")
  "m e" '(:ignore t :which-key "[E]val")
  "m e e" '(eval-last-sexp :which-key "[E]val sexp")
  "m e s" '(org-babel-execute-src-block :which-key "[E]val [s]rc block")
  "m ," '(org-insert-structure-template :which-key "inserc template")
  "m <" '(org-do-promote :which-key "<- promote")
  "m >" '(org-do-demote :which-key "demote ->")
  "m o" '(org-open-at-point :which-key "[O]pen link at point")
  "m i l" '(org-insert-link :which-key "[I]nsert [l]ink")
  "m i i" '(org-insert-item :which-key "[I]nsert [i]tem")
  "m i n" '(dm/org-insert-name :which-key "[I]nsert [n]name"))

(general-define-key
 :states 'motion
 :keymaps 'markdown-mode-map
 "<tab>" 'markdown-cycle)

(leader-def
  :keymaps 'emacs-lisp-mode-map
  "m" '(:ignore t :which-key "[M]AJOR MODE KEYMAPS")
  "m e" '(:ignore t :which-key "[E]val")
  "m e e" '(eval-last-sexp :which-key "[E]val [e]xpression"))

(leader-def
  :keymaps 'tuareg-mode-map
  "m" '(:ignore t :which-key "[M]AJOR MODE KEYMAPS")
  "m e" '(:ignore t :which-key "[E]val")
  "m e e" '(utop-eval-phrase :which-key "[E]val [e]xpression")
  "m e r" '(utop-eval-region :which-key "[E]val [r]egion")
  "m e b" '(utop-eval-buffer :which-key "[E]val [B]uffer")
  "m c" '(tuareg-comment-dwim :which-key "[C]omment line"))

(leader-def
  "k" '(:ignore t :which-key "sexp")
  "k w" '(sp-wrap-round :which-key "[W]rap ()")
  "k u" '(sp-unwrap-sexp :which-key "[U]nwrap sexp")
  "k [" '(sp-wrap-square :which-key "[W]rap []")
  "k {" '(sp-wrap-curly :which-key "[W]rap {}")
  "k ," '(sp-forward-barf-sexp :which-key "<-)")
  "k ." '(sp-forward-slurp-sexp :which-key ")->")
  "k <" '(sp-backward-barf-sexp :which-key "<-(")
  "k >" '(sp-forward-barf-sexp :which-key "(->")
  "k r" '(sp-raise-sexp :which-key "[R]aise sexp"))

(leader-def
  "f" '(:ignore t :which-key "[F]ile")
  "f f" '(counsel-find-file :which-key "[F]ind file")
  "f g" '(counsel-git :which-key "counsel [g]it")
  "f s" '(save-buffer :which-key "[S]ave file"))

(leader-def
  "p" '(:ignore t :which-key "[P]roject")
  "p f" '(projectile-find-file :which-key "[F]ind file in project")
  "p d" '(projectile-find-dir :which-key "find [d]ir in project")
  "p s" '(counsel-git-grep :which-key "[S]earch for occurencies"))

(leader-def
  "g" '(:ignore t :which-key "[G]it")
  "g s" '(magit-status  :which-key "magit [s]tatus"))

(use-package move-text
  :ensure t
  :demand t
  :init
  :config)

(general-define-key
 :states '(motion normal visual)
 :keymaps 'override
 "M-k" 'move-text-up
 "M-j" 'move-text-down)

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package add-node-modules-path
  :ensure t
  :init)

(use-package eslint-fix)

(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

(setq-default tab-width 4)
