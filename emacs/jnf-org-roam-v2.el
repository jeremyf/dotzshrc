;;; jnf-org-roam-v2.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  This package provides configuration for org-roam
;;
;;; Code:

;; A Property List of my `org-roam' capture templates.
(setq jnf/org-roam-capture-templates-plist
      (list
       :thel-sector '("t" "Projects > Thel Sector" plain "%?"
                      :if-new (file+head "projects/thel-sector/%<%Y%m%d>---${slug}.org"
                                         "#+title: ${title}\n#+FILETAGS: :thel-sector: %^G\n\n")
                      :unnarrowed t)
       :personal '("p" "Personal" plain "%?"
		   :if-new (file+head "personal/%<%Y%m%d>---${slug}.org"
				      "#+title: ${title}\n#+FILETAGS: :personal: %^G\n\n")
		   :unnarrowed t)
       :public '("u" "Public" plain "%?"
		 :if-new (file+head "public/%<%Y%m%d>---${slug}.org"
				    "#+title: ${title}\n#+FILETAGS: :public: %^G\n\n")
		 :unnarrowed t)
       ))

(defun jnf/org-roam-templates-for (&rest symbols)
  "Return a list of `org-roam' templates for the given SYMBOLS."
  (-map (lambda (symbol) (plist-get jnf/org-roam-capture-templates-plist symbol))
        symbols))

(defun jnf/org-roam-capture--thel-sector (&optional goto)
  "Capture a Thel Sector entry.

When GOTO is non-nil, go to the note without creating an entry."
  (interactive)
  (org-roam-capture- :goto (when goto '(4))
                     :node (org-roam-node-read
                            ;; Initial text.  Use region if one is selected, otherwise the given word.
                            (if (use-region-p)
                                (buffer-substring-no-properties (region-beginning) (region-end))
                              (thing-at-point 'word))
                            ;; Filter function
                            (lambda (node) (-contains-p (org-roam-node-tags node) "thel-sector")))
                     :props '(:immediate-finish nil)
                     :templates (jnf/org-roam-templates-for :thel-sector)))

;; With the latest update of org-roam, things again behavior
;; correctly.  Now I can just load org-roam as part of my day to day
(use-package org-roam
  :straight t
  :custom
  (org-roam-directory (file-truename "~/git/org"))
  ;; Set more spaces for tags; As much as I prefer the old format,
  ;; this is the new path forward.
  (org-roam-node-display-template "${title:*} ${tags:40}")
  (org-roam-capture-templates (jnf/org-roam-templates-for
                               :personal
                               :public
			       :thel-sector))
  :init
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))

  (setq org-roam-v2-ack t)
  :bind (("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ;; Dailies
         ("C-c r j" . org-roam-dailies-capture-today)))

(org-roam-setup)
(provide 'jnf-org-roam-v2.el)
;;; jnf-org-roam-v2.el ends here
