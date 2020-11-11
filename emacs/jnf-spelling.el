;;; -*- lexical-binding: t; -*-
;;; package --- Summary
;;
;;; Commentary:
;;
;;  This package provides configuration for spell checking.  It
;;  assumes the use of ivy; See the flyspell-correct-ivy package.
;;
;;  By default, I don't render hints for spell checking nor grammar
;;  checking.  Instead I rely on two checks:
;;
;;  1. flyspell-buffer (via kbd "C-,")
;;  2. writegood-mode (via kbd "C-c w")
;;
;;  Once I've called flyspell-buffer, I can then navigate through
;;  identified issues and address them via flyspell-popup-correct (via
;;  kbd "C-").
;;
;;  For writegood-mode, I
;;
;;; Code:
(use-package flycheck
  :straight t
  :hook
  (org-src-mode . disable-flycheck-for-elisp)
  :custom
  (flycheck-display-errors-delay 0.1)
  (flycheck-emacs-lisp-initialize-packages t)
  :config
  (flycheck-set-indication-mode 'left-margin)

  (defun disable-flycheck-for-elisp ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

  (add-to-list 'flycheck-checkers 'proselint))
(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package flyspell-correct
  :straight t
  )

(use-package flyspell-correct-ivy
  :straight t
  :config (global-set-key (kbd "C-,") 'flyspell-buffer))

;; Run flyspell-buffer
(use-package flyspell-popup
  :straight t
  :bind (("s-4" . #'flyspell-popup-correct))
  :hook (flyspell-mode . flyspell-popup-auto-correct-mode))

;; For grammar nerd
(use-package writegood-mode
  :straight t
  :bind ("C-c w" . writegood-mode)
  :config
  (add-to-list 'writegood-weasel-words "actionable"))
(provide 'jnf-spelling.el)
;;; jnf-spelling.el ends here
