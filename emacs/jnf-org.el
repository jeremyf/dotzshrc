;;; jnf-org.el --- Summary
;;
;;; Commentary:
;;
;;  This package provides configuration for org-roam
;;
;;; Code:
;; ;; Consider https://github.com/jkitchin/org-ref as well

(use-package org-sidebar
  :straight (org-sidebar :type git
                         :host github
                         :repo "alphapapa/org-sidebar")
  :bind (
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c t" . org-toggle-link-display)
         ("s-2" . jnf-org-insert-immediate-inactive-timestamp)))

;; Uncomment to always launch org mode with a sidebar tree
;; (add-hook 'org-mode-hook #'org-sidebar-tree)

(setq org-directory "~/git/org")
(setq org-agenda-files (list "~/git/org"))
(setq org-default-notes-file (concat org-directory "/captured-notes.org"))

;; To make Org mode take care of versioning of attachments for you,
;; add the following to your Emacs config:
;;
(require 'org-attach-git)

;; I'm working through what templates I might want. This is a place holder.
(setq org-capture-templates
      '(
        ("c" "Write to Current Clock" plain (clock)
         "%?\n %a")
        ("m" "Meeting for Work" entry (file+datetree "~/git/org/agenda.org")
         "* MEETING %^{SUMMARY}\n\n  %^{ATTENDEES}p\n  %?\n")
        ("r" "Reading for Work" entry (file+datetree "~/git/org/agenda.org")
         "* TO-READ %^{SUBJECT} %u\n  %?\n")
        ("s" "Session" entry (file+headline "~/git/org/sessions.org" "Sessions")
         "* Session: %u %^{SUMMARY}\n\n  %^{ATTENDEES}p\n  %^{SYSTEM}p\n  %?\n")
        ("g" "Troubleshooting" entry (file+headline "~/git/org/troubleshooting.org" "Trouble Shooting")
         "* TODO %u Problem %^{SUMMARY}\n\n  %?\n  %a")
        ("t" "Task for Work" entry (file+datetree "~/git/org/agenda.org")
         "* TODO %?")
        ("u" "Unfiled Permanent > Bibliography" entry (file+headline "~/git/org/permanent/unfiled_bibliographic_cards.org" "Unfiled Bibliographic Cards")
         "* UNFILED %?\nEntered on %U")
        ("w" "Waiting for Work" entry (file+datetree "~/git/org/agenda.org")
         "* WAITING %^{SUMMARY}\n\n  %?\n")
        ))


;; Different key words and state machines help contextual the work.
;;
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE")
        (sequence "MEETING" "AGENDA" "|" "MINUTES")
        (sequence "UNFILED" "|" "FILED")
        (sequence "TO-READ" "READING" "|" "READ"))
      )

(use-package org-web-tools
  :straight t)

(use-package org-d20
  :straight t)


(defalias 'roll 'org-d20-roll-at-point)

(use-package org-superstar
  :straight t
  :hook ((org-mode . org-superstar-mode)
         (org-mode . turn-on-visual-line-mode)))

;; See
;; https://www.reddit.com/r/orgmode/comments/i6hl8b/image_preview_size_in_org_mode/
;; for further discussion
;;
;; One consideration is that the below setq should be called as part
;; of the `org-toggle-inline-images`
(setq org-image-actual-width (truncate (* (window-pixel-width) 0.8)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)))

(defun gorg (&optional org_file_basename)
  "Jump to the given ORG_FILE_BASENAME or toggle it's org-sidebar.

If no ORG_FILE_BASENAME is given default to `agenda.org'. I chose
`gorg' as the mnemonic Goto Org."
  (interactive)
  ;; Need to know the location on disk for the buffer
  (unless org_file_basename (setq org_file_basename "agenda.org"))
  (setq org_filename (concat org-directory "/" org_file_basename))
  (let ((current_filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (if (equal current_filename (expand-file-name org_filename))
        (progn (org-sidebar-toggle))
      (progn (find-file org_filename) (delete-other-windows)))))

(global-set-key (kbd "<f2>") 'gorg)
(global-set-key (kbd "<f5>") `(lambda () (interactive) (gorg "troubleshooting.org")))


;; Insert immediate timestamp
(defun jnf-org-insert-immediate-inactive-timestamp ()
  "Inserts a timestamp with a single button press for the current time."
  (interactive)
  (org-insert-time-stamp nil t nil " " " "))

;; For some reason, when I load emacs in daemon mode, the daemon
;; process is the process now renders the GET prompts for the
;; mini-buffer.  When I load the file interactively, I don't
;; experience the same problem.  So, until this resolves, I'll need to
;; load roam via an interactive command.
;; (global-set-key (kbd "<f10>") `(lambda ()
;;                                 (interactive)
;;                                 (require 'jnf-org-roam.el)
;;                                 ))

(require 'jnf-org-roam.el)

(provide 'jnf-org.el)
;;; jnf-org.el ends here
