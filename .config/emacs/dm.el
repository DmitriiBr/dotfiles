;;; package --- Summary: dm/modules  -*- lexical-binding: t; -*-
;;; Commentary:
;;; dm/modules! funciton, that enables or disables preconfigured
;;; in `config.org` modules

;; `dm/modules!` args
;; `:completion` - Packages for completion
;; `:tools` - Tooling packages for VCS, LSP (for now)

;;; Code:

(setq modules-hash-table (make-hash-table :test 'eq))

(defmacro dm/use-package! (name &rest args)
  `(puthash ',name '(,@args) modules-hash-table))

(defmacro dm/modules! (&rest modules)
  "Modules loading macro"
  `(let ((modules-symbols (car (cl-remove-if #'keywordp '(,@modules)))))
	 (use-package '(symbol-value 'modules-symbols))
	 ;;	 (dolist (package-key modules-symbols nil)
	 ;;	   (use-package package-key))
	 ;;		 (gethash package-key modules-hash-table)))
     ))

(org-babel-load-file "~/.config/emacs/config.org")

;; (dm/modules! :completion ivy)

;; (dm/modules!
;;  :completion
;;  ivy
;;  ido
;;  ;; vertico

;;  :tools
;;  ;; lsp
;;  magit

;;  :ui
;;  mood-line

;;  :lang
;;  emacs-lisp
;;  (javascript :formatter prettier :lsp nil)
;;  web
;;  org
;;  ocaml 
;;  json
;;  markdown
;;  sh

;;  :checkers
;;  flymake

;;  :config
;;  (default +bindings +smartparens))

(provide 'dm)
;;; dm.el ends here
