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
  :init (setq git-commit-fill-column 72) ;; Adding format to
                                         ;; git-commit-fill-column of
                                         ;; 72 as best practice.
  :bind (("H-g" . magit-status)
         ("C-M-g" . magit-status)))

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

;; Copied from
;; https://github.com/magit/magit/blob/9423edc0b311117ab5fe87457c3e01c7db22a3c7/lisp/git-commit.el
;; And set to 50 instead of 68, because good form is 50 characters.
(defcustom git-commit-summary-max-length 50
  "A reminder of an aspirational goal of terse git commit summary
lines."
  :group 'git-commit
  :safe 'numberp
  :type 'number)

(provide 'jnf-git.el)
;;; jnf-git.el ends here
