;;; jnf-lsp-mode.el --- Summary
;;
;;; Commentary:
;;
;;  This package provides the lsp-mode behavior.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq lsp-keymap-prefix "C-l")
(use-package lsp-mode
  :straight t
  :hook ((ruby-mode . lsp))
  :commands (lsp))


;; This package provides some nice UI behavior for documentation and linting
;;
;; In particular, I like 'lsp-ui-peek-find-reference
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook ((ruby-mode . lsp-ui-mode)
         (ruby-mode . lsp-ui-peek-mode)
         (ruby-mode . lsp-ui-sideline-mode))
  :straight t)

(use-package lsp-ivy
  :after ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package company-lsp
  :straight t
  :custom (company-lsp-enable-snippet t)
  :after (company lsp-mode))

(add-hook 'emacs-lisp-mode 'company-mode)
(add-hook 'emacs-lisp-mode 'eldoc-mode)

;; Solargraph is the language tool for lsp-mode and Ruby
(use-package solargraph
  :straight (solargraph :host github :repo "guskovd/emacs-solargraph"))

(define-key ruby-mode-map (kbd "M-i") 'solargraph:complete)
(provide 'jnf-lsp-mode.el)
;;; jnf-lsp-mode.el ends here
