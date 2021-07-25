;;; jnf-org-roam-v2.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  This package provides configuration for org-roam
;;
;;; Code:

(setq jnf/org-roam-capture-templates--thel-sector
      '("t" "Thel Sector" plain "%?"
        :if-new (file+head "projects/thel-sector/%<%Y%m%d>---${slug}.org"
                           "#+title: ${title}\n#+FILETAGS: :thel-sector: %^G\n\n")
        :unnarrowed t))

(defun jnf/org-roam-capture--thel-sector (&optional goto)
  "Capture a Thel Sector entry.

When GOTO is non-nil, go the note without creating an entry."
  (interactive)
  (org-roam-capture- :goto (when goto '(4))
                     :node (org-roam-node-read)
                     :props '(:immediate-finish nil)
                     :templates (list jnf/org-roam-capture-templates--thel-sector)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN ORG ROAM  and concerns
;;
;; With the latest update of org-roam, things again behavior
;; correctly.  Now I can just load org-roam as part of my day to day
(use-package org-roam
  ;; :straight (org-roam :type git :host github :repo "org-roam/org-roam" branch: "v2")
  :straight t
  :custom
  (org-roam-directory (file-truename "~/git/org"))
  ;; Set more spaces for tags; As much as I prefer the old format,
  ;; this is the new path forward.
  (org-roam-node-display-template "${title:*} ${tags:40}")
  (org-roam-capture-templates '(
				("p" "Personal" plain "%?"
                                 :if-new (file+head "personal/%<%Y%m%d>---${slug}.org"
                                                    "#+title: ${title}\n#+FILETAGS: :personal: %^G\n\n")
                                 :unnarrowed t)
				("u" "Public" plain "%?"
                                 :if-new (file+head "public/%<%Y%m%d>---${slug}.org"
                                                    "#+title: ${title}\n#+FILETAGS: :public: %^G\n\n")
                                 :unnarrowed t)
				))
  :init
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  :bind (("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ;; Dailies
         ("C-c r j" . org-roam-dailies-capture-today)))

(setq org-roam-v2-ack t)
(org-roam-setup)
(provide 'jnf-org-roam-v2.el)
;;; jnf-org-roam-v2.el ends here
