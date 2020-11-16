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
  :hook (
         (ruby-mode . lsp)
         (js-mode . lsp)
         (typescript-mode . lsp)
         )
  :commands (lsp))


;; This package provides some nice UI behavior for documentation and linting
;;
;; In particular, I like 'lsp-ui-peek-find-reference
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (
         (ruby-mode . lsp-ui-mode)
         (ruby-mode . lsp-ui-peek-mode)
         (ruby-mode . lsp-ui-sideline-mode)
         (typescript-mode . lsp-ui-mode)
         (typescript-mode . lsp-ui-peek-mode)
         (typescript-mode . lsp-ui-sideline-mode)
         (js-mode . lsp-ui-mode)
         (js-mode . lsp-ui-peek-mode)
         (js-mode . lsp-ui-sideline-mode)
         )
  :straight t)

;; By default indent levels are often 4; That is against what I've seen.
(setq ruby-indent-level 2
      typescript-indent-level 2
      js-indent-level 2)

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
  :straight (solargraph :host github :repo "guskovd/emacs-solargraph")
  :bind (:map ruby-mode-map ("M-i" . solargraph:complete)))

(provide 'jnf-lsp-mode.el)
;;; jnf-lsp-mode.el ends here
