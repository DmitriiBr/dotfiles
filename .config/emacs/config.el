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

;;; Runtime optimizations
;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; PERF: Disable bidirectional text scanning for a modest performance boost.
;;   I've set this to `nil' in the past, but the `bidi-display-reordering's docs
;;   say that is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses (and other
;;   bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; PGTK builds only: there's a timeout that adds latency to frame operations,
;; like `make-frame-invisible', which Emacs frequently calls without a guard
;; because it's inexpensive in non-PGTK builds. Lowering the timeout from the
;; default 0.1 should make childframes and packages that manipulate them (like
;; `lsp-ui', `company-box', and `posframe') feel much snappier. See
;; emacs-lsp/lsp-ui#613.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil    ; decrease file IO workload
        w32-pipe-read-delay 0               ; faster IPC
        w32-pipe-buffer-size (* 64 1024)))  ; read more at a time (was 4K)

;; The GC introduces annoying pauses and stuttering into our Emacs experience,
;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
;; when it's idle. However, if the idle delay is too long, we run the risk of
;; runaway memory usage in busy sessions. And if it's too low, then we may as
;; well not be using gcmh at all.
(setq gcmh-idle-delay 'auto  ; default is 15s
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb

  ;;; Disable UI elements early
;; PERF,UI: Doom strives to be keyboard-centric, so I consider these UI elements
;;   clutter. Initializing them also costs a morsel of startup time. What's
;;   more, the menu bar exposes functionality that Doom doesn't endorse or
;;   police. Perhaps one day Doom will support these, but today is not that day.
;;   By disabling them early, we save Emacs some time.

;; HACK: I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;;   `scroll-bar-mode' because their manipulation of frame parameters can
;;   trigger/queue a superfluous (and expensive, depending on the window system)
;;   frame redraw at startup. The variables must be set to `nil' as well so
;;   users don't have to call the functions twice to re-enable them.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

  ;;; Encodings
;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; ...but `set-language-environment' also sets `default-input-method', which is
;; a step too opinionated.
(setq default-input-method nil)
;; ...And the clipboard on Windows is often a wider encoding (UTF-16), so leave
;; Emacs to its own devices there.
(unless (or 'cygwin 'windows-nt 'ms-dos)
  (setq selection-coding-system 'utf-8))

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
  (setq evil-insert-state-cursor '(bar . 3))
  (setq evil-move-beyond-eol nil)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer magit))
  (evil-collection-init))

;; (ido-mode 1)
;; (ido-everywhere 1)
;; (setq ido-separator "\n")

(use-package ivy
  :disabled t
  :ensure t
  :demand t
  :config
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (ivy-mode +1)
  (ivy-prescient-mode +1))

(use-package counsel
  :disabled t
  :ensure t
  :demand t
  :config (counsel-mode +1))

(use-package vertico
  :ensure t
  :custom;
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 8) ;; Show more candidates
  (vertico-resize nil) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC"))

(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode +1))

(setq which-key-idle-delay 0.5)
(setq which-key-idle-secondary-delay 0)

(use-package projectile
  :disabled t
  :ensure t
  :demand t
  :config
  (projectile-mode +1))

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

(setq dm/is-windows-system (not (or (eq system-type 'gnu/linux) (eq system-type 'darwin))))

(use-package apheleia
  :if (not dm/is-windows-system)
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

(use-package prettier
  :if dm/is-windows-system
  :config (prettier-global-mode +1))

(use-package flymake
  :ensure t
  :config (flymake-mode t)
  :hook (emacs-lisp-mode typesript-mode))

(use-package lsp-mode
  :disabled t
  :ensure t
  :defer t
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
  :disabled t
  :ensure t
  :defer t
  :init
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-max-width 70)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  :commands lsp-ui-mode)

(evil-ex-define-cmd "q" 'kill-current-buffer)
(evil-ex-define-cmd "wq" (lambda () 
			   (interactive)
			   (save-buffer)
			   (kill-current-buffer)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general :ensure t)

(general-create-definer leader-def
  :states '(normal insert motion visual emacs)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

(leader-def
  "SPC" '(execute-extended-command :which-key "M-x")
  "w s m" '(which-key-show-major-mode :which-key "[W]K [M]AJOR MODE KEYMAPS"))

(general-define-key
 :states '(visual normal motion)
 :keymaps 'override
 "$" 'evil-last-non-blank
 "-" 'evil-last-non-blank)

(general-define-key
 :keymaps 'global
 "C-," (lambda ()
         (interactive)
         (duplicate-line)
         (next-line)))

(leader-def
  "b" '(:ignore t :which-key "[B]uffer")
  "b s" '(consult-buffer :which-key "[S]witch to buffer")
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
  "m i n" '(dm/org-insert-name :which-key "[I]nsert [n]name")
  "m x f" '(org-emphasize :which-key "Emphasize C-c C-x C-f"))

(general-define-key
 :states 'motion
 :keymaps 'markdown-mode-map
 "<tab>" 'markdown-cycle)

(leader-def
  :keymaps 'emacs-lisp-mode-map
  "m" '(:ignore t :which-key "[M]AJOR MODE KEYMAPS")
  "m e" '(:ignore t :which-key "[E]val")
  "m e e" '(eval-last-sexp :which-key "[E]val [e]xpression")
  "m e r" '(eval-region :which-key "[E]val [r]region"))

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
  "f f" '(find-file :which-key "[F]ind file")
  "f s" '(save-buffer :which-key "[S]ave file"))

(leader-def
  "p" '(:ignore t :which-key "[P]roject")
  "p f" '(project-find-file :which-key "[F]ind file in project")
  "p d" '(project-find-dir :which-key "find [d]ir in project")
  "p s" '(consult-git-grep :which-key "[S]earch for occurencies")
  "p b" '(:ignore t :which-key "[B]uffers")
  "p b s" '(consult-project-buffer :which-key "[S]witch to buffer"))

(leader-def
  "g" '(:ignore t :which-key "[G]it")
  "g s" '(magit-status  :which-key "magit [s]tatus"))

(use-package move-text
  :ensure t)

(general-define-key
 :states '(motion normal visual)
 :keymaps 'override
 "M-k" 'move-text-up
 "M-j" 'move-text-down)

(if dm/is-windows-system (add-to-list 'exec-path "c:/Program Files/Git/usr/bin"))

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
