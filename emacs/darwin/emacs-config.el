;;; package --- Summary
;;
;;; Commentary:
;;
;;  This package loads darwin specific packages; It assumes that both
;;  "use-package" and "straight-use-package" are loaded.
;;
;;; Code:

;; I have set this in OS X, however without a fallback, on Linux the
;; interpretter halts here
(set-frame-font "MesloLGS NF 13" nil t)

;; Adds the ability to grab a link from various OS X applications
;; Note, the sibling org-mac-link.  That package works within ORG mode
;; with an extended menu option, and assumes ORG styling.  They both
;; have the same keybinding as org-mode favors org-mac-link.
(use-package grab-mac-link
  :ensure t
  :straight t
  :defer 1)
(global-set-key (kbd "C-c C-g") 'grab-mac-link)

(use-package org-mac-link
  :ensure t
  :straight (org-mac-link :type git :host github :repo "jeremyf/org-mac-link")
  :defer t)
(add-hook 'org-mode-hook (lambda ()
                           (define-key org-mode-map (kbd "C-c C-g") 'org-mac-grab-link)))

;
;; Keep this after `counsel' so that the key binding is
;;  overridden only on OSX."
(use-package counsel-osx-app
    :ensure t
    :straight t
    :after counsel
    :bind ("C-c C-l" . counsel-osx-app))


;; On  I use ⌘ as meta and prefer ⌥ to do nothing so I can still insert special characters easily.
;;
;; (setq mac-command-modifier 'meta
;;       mac-option-modifier 'none)

(provide 'emacs-config.el)
;;; emacs-config.el ends here
