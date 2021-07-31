;;; jnf-config.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Provides some configuration information for Emacs.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq jnf/data-directories '(
                             ;; The themes directory
                             "~/git/takeonrules.github.io/themes/hugo-tufte"
                             ;; The content directory
                             "~/git/takeonrules.github.io/"
                             ;; The tooling directory
                             "~/git/dotzshrc/"
                             ;; The personal configuration options
                             "~/git/jnf-emacs-config/"
                             ;; An org directory
                             "~/git/org/"
                             ;; An org directory
                             "~/git/org/archive"
                             ;; An org directory
                             "~/git/org/daily"
                             ;; An org directory
                             "~/git/org/public"
                             ;; An org directory
                             "~/git/org/personal"
                             ;; An org directory
                             "~/git/org/hesburgh-libraries"
                             ))

(defun jnf/data-sync ()
  "Synchronize data repositories."
  (interactive)
  (message "Synchronizing local git repos...")
  (dolist (path jnf/data-directories)
    (if (f-dir-p (file-truename path))
        (progn
          (message (concat "Syncing \"" path "\"..."))
          (shell-command-to-string
           (concat
            "cd " path
            " && git pull --rebase"
            " && git push -u --force-with-lease")))
      (message (concat "Skipping missing directory \"" path "\"...")))
    (message "Finished synchronizing local git repos.")))


(defun jnf/git-statuses ()
  "Synchronize data repositories."
  (interactive)
  (message "Review status of local git repos...")
  (dolist (path jnf/data-directories)
    (if (f-dir-p (file-truename path))
        (magit-status path))))

(provide 'jnf-config.el)
;;; jnf-config.el ends here
