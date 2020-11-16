;;; -*- lexical-binding: t; -*-
;;; jnf-git.el --- Summary
;;
;;; Commentary:
;;
;;  This package includes the various configurations for git
;;  interactions.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The OMG awesome git client for emacs.
(use-package magit
  :straight t
  :init (setq git-commit-fill-column 72) ;; Adding format to git-commit-fill-column of 72 as best practice.
  (setq git-commit-summary-max-length 50)
  :bind (("C-c m" . magit-status)
         ("C-c RET" . magit-file-dispath))
  :init
  ;; Have magit-status go full screen and quit to previous
  ;; configuration.  Taken from
  ;; http://whattheemacsd.com/setup-magit.el-01.html#comment-748135498
  ;; and http://irreal.org/blog/?p=2253
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice magit-mode-quit-window (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen))
  :config
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header))

(use-package forge
  :straight t)

(use-package libgit
  :straight t)

(use-package magit-libgit
  :straight t)

;; I believe this means I should first ensure that I've loaded ivy.
(with-eval-after-load "magit"
  (setq magit-completing-read-function 'ivy-completing-read))

;; With the time machine, travel back and forth through a files history.
;;
;; While visiting a point in time, you can open
(use-package git-timemachine
  :straight t)

;; Show the current git state in the gutter
(use-package git-gutter
  :straight t
  :config (global-git-gutter-mode 't))

;; https://github.com/sshaw/git-link
;;
;; `M-x git-link` to add the current URL to the kill ring.  This is
;; particularly helpful for sharing links with other developers.  I
;; use this ALL OF THE TIME
(use-package git-link
	     :config (setq git-link-use-commit t) ;; URL will be SHA instead of branch
	     :straight t)

(use-package git-messenger
  :config (setq git-messenger:show-detail t)
  :custom (git-messenger:use-magit-popup t)
  :bind (("s-6" . git-messenger:popup-message))
  :straight t)

(provide 'jnf-git.el)
;;; jnf-git.el ends here