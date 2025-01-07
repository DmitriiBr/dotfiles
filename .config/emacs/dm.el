;;; package --- Summary: dm/modules  -*- lexical-binding: t; -*-
;;; Commentary:
;;; dm/modules! funciton, that enables or disables preconfigured
;;; in `config.org` modules

;; `dm/modules!` args
;; `:completion` - Packages for completion
;; `:tools` - Tooling packages for VCS, LSP (for now)

;;; Code:

(defun modules-to-string (modules-list) (mapconcat (lambda (x) (symbol-name x)) modules-list " "))

(defmacro dm/modules! (&rest modules)
  "Modules string"
  ;; (let ((ivy (plist-get modules ivy))) ivy)
  ;; (split-string (modules-to-string modules) ":")
  ;; modules-string
  `(split-string (modules-to-string '(list ,@modules)) ":"))


(dm/modules!
 :completion
 ivy
 ido

 :tools
 magit)



(dm/modules!

 :completion
 ivy
 ido
 ;; vertico

 :tools
 ;; lsp
 magit

 :ui
 mood-line

 :lang
 emacs-lisp
 (javascript :formatter prettier :lsp nil)
 web
 org
 ocaml 
 json
 markdown
 sh

 :checkers
 flymake

 :config
 (default +bindings +smartparens))


(provide 'dm)
;;; dm.el ends here
