;;; jnf-org-roam-v2.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  This package provides configuration for org-roam
;;
;;; Code:

(defun jnf/org-roam-capture-templates ()
  "Assign the capture templates.

TODO: I would like to use a directory's files to assign these
values.  However, my elisp skills are not yet up for that.  So I'm
going get this working and seek help online."
  (setq jnf/org-roam-capture-templates-list '(
                    ("p" "Public" plain "%?"
                                 :if-new (file+head "public/%<%Y%m%d>-${slug}.org"
                                                    "#+title: ${title}\n#+FILETAGS: :public: %^G\n\n")
                                 :unnarrowed t)
                    ))
  (add-to-list 'jnf/org-roam-capture-templates-list
    '("n" "Notre Dame" plain "%?"
     :if-new (file+head "notre-dame/%<%Y%m%d>-${slug}.org"
                        "#+title: ${title}\n#+FILETAGS: :notre-dame: %^G\n\n")
     :unnarrowed t))
  jnf/org-roam-capture-templates-list)

;; (defun jnf/capture-template-for-directory (dir)
;;   (let* ((subdir (format "%s" (car (last (f-split dir)))))
;;          (prefix (substring subdir 0 1)))
;;     (list prefix (s-titleize subdir) plain)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN ORG ROAM  and concerns
;;
;; With the latest update of org-roam, things again behavior
;; correctly.  Now I can just load org-roam as part of my day to day
(use-package org-roam
  :straight (org-roam :type git :host github :repo "org-roam/org-roam" branch: "v2")
  :custom
  (org-roam-directory (file-truename "~/git/org-roam"))
  ;; Set more spaces for tags; As much as I prefer the old format,
  ;; this is the new path forward.
  (org-roam-node-display-template "${title:*} ${tags:40}")
  (org-roam-capture-templates (jnf/org-roam-capture-templates))
  :bind (("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r g" . org-roam-graph)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ;; Dailies
         ("C-c r j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
  (require 'org-roam-protocol))


(provide 'jnf-org-roam-v2.el)
;;; jnf-org-roam-v2.el ends here
