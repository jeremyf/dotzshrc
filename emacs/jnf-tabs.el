;;; -*- lexical-binding: t; -*-
;;; jnf-tabs.el --- Summary
;;
;;; Commentary:
;;
;;  This package provides the tabs behavior.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-tab-line-mode t)
;; (global-set-key (kbd "s-{") 'previous-buffer)
;; (global-set-key (kbd "s-}") 'next-buffer)

;; "The long-awaited Emacs 27 support for native tabs is shaky, both
;; visually and in terms of functionality. As such, centaur-tabs is
;; the best way to simulate a conventional tabs setup, in which tab
;; sets are grouped by the toplevel project working directory."
;; https://blog.sumtypeofway.com/posts/emacs-config.html
(use-package centaur-tabs
  :straight t
  :demand
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-show-navigation-buttons t
        centaur-tabs-set-icons t
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-set-bar 'under)
  (centaur-tabs-headline-match)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((string-equal "*elf" (substring (buffer-name) 0 4)
       "RSS")
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
	   (memq major-mode '(magit-process-mode
			      magit-status-mode
			      magit-diff-mode
			      magit-log-mode
			      magit-file-mode
			      magit-blob-mode
			      magit-blame-mode
			      )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
			  help-mode))
       "Help")
      ((memq major-mode '(org-mode
			  org-agenda-clockreport-mode
			  org-src-mode
			  org-agenda-mode
			  org-beamer-mode
			  org-indent-mode
			  org-bullets-mode
			  org-cdlatex-mode
			  org-agenda-log-mode
			  diary-mode))
       "Editing")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :custom
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-change-fonts "JetBrains Mono" 90)
  (centaur-tabs-style "rounded")
  (centaur-tabs-height 36)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "●")
  ;; (centaur-tabs-buffer-groups-function #'centaur-tabs-projectile-buffer-groups)
  :bind (("s-{" . #'centaur-tabs-backward)
         ("s-}" . #'centaur-tabs-forward)
         ("C-c C-5". #'centaur-tabs-extract-window-to-new-frame)
         ([s-up] . #'centaur-tabs-backward-group)
         ([s-down] . #'centaur-tabs-forward-group)
         ("C-s-t" . #'centaur-tabs-counsel-switch-group)
         ("C-c C-o" . #'centaur-tabs-open-directory-in-external-application)))


(provide 'jnf-tabs.el)
;;; jnf-tabs.el ends here
