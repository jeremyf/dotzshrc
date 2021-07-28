;;; jnf-org-roam-v2.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  This package provides configuration for org-roam
;;
;;; Code

(defun jnf/org-roam-templates-for (&rest symbols)
  "Return a list of `org-roam' templates for the given SYMBOLS."
  (-map (lambda (symbol) (plist-get jnf/org-roam-capture-templates-plist symbol))
        symbols))

(defmacro create-org-roam-capture-fn-for (project)
  "Define a `jnf/org-roam-capture' function for PROJECT."
  (let* ((project-as-symbol (intern (concat ":" project)))
         (fn-name (intern (concat "jnf/org-roam--" project "--capture")))
         (docstring (concat "As `jnf/org-roam-capture' but scoped to " project
                            ".\n\nArguments GOTO and KEYS see `org-capture'.")))
    `(defun ,fn-name (&optional goto keys)
       ,docstring
       (interactive "P")
       (org-roam-capture goto
                             keys
                             :filter-fn (lambda (node) (-contains-p (org-roam-node-tags node) ,project))
                             :templates (jnf/org-roam-templates-for ,project-as-symbol)))))

(defmacro create-org-roam-node-insert-fn-for (project)
  "Define a `jnf/org-roam-node-insert' function for PROJECT."
  (let* ((project-as-symbol (intern (concat ":" project)))
         (fn-name (intern (concat "jnf/org-roam--" project "--node-insert")))
         (docstring (concat "As `jnf/org-roam-insert-node' but scoped to " project " project.")))
      `(defun ,fn-name ()
         ,docstring
         (interactive)
         (org-roam-node-insert (lambda (node) (-contains-p (org-roam-node-tags node) ,project))
                                   :templates (jnf/org-roam-templates-for ,project-as-symbol)))))

(defmacro create-org-roam-node-find-fn-for (project)
  "Define a `jnf/org-roam-node-find' function for PROJECT."
  (let* ((project-as-symbol (intern (concat ":" project)))
         (fn-name (intern (concat "jnf/org-roam--" project "--node-find")))
         (docstring (concat "As `jnf/org-roam-find-node' but scoped to "
                            project " project."
                            "\n\nArguments INITIAL-INPUT and OTHER-WINDOW are from `org-roam-find-mode'.")))
      `(defun ,fn-name (&optional other-window initial-input)
         ,docstring
         (interactive current-prefix-arg)
         (org-roam-node-find other-window
                                 initial-input
                                 (lambda (node) (-contains-p (org-roam-node-tags node) ,project))
                                 :templates (jnf/org-roam-templates-for ,project-as-symbol)))))

(create-org-roam-capture-fn-for "thel-sector")
(create-org-roam-node-insert-fn-for "thel-sector")
(create-org-roam-node-find-fn-for "thel-sector")

(create-org-roam-capture-fn-for "hesburgh-libraries")
(create-org-roam-node-insert-fn-for "hesburgh-libraries")
(create-org-roam-node-find-fn-for "hesburgh-libraries")

;; A Property List of my `org-roam' capture templates.
(setq jnf/org-roam-capture-templates-plist
      (list
       :hesburgh-libraries '("h" "Hesburgh Libraries" plain "%?"
		   :if-new (file+head "org-hesburgh-libraries/%<%Y%m%d>---${slug}.org"
				      "#+title: ${title}\n#+FILETAGS: :hesburgh: %^G\n\n")
		   :unnarrowed t)
       :personal '("p" "Personal" plain "%?"
		   :if-new (file+head "personal/%<%Y%m%d>---${slug}.org"
				      "#+title: ${title}\n#+FILETAGS: :personal: %^G\n\n")
		   :unnarrowed t)
       :public '("u" "Public" plain "%?"
		 :if-new (file+head "public/%<%Y%m%d>---${slug}.org"
				    "#+title: ${title}\n#+FILETAGS: :public: %^G\n\n")
		 :unnarrowed t)
       :thel-sector '("t" "Thel Sector" plain "%?"
                      :if-new (file+head "personal/thel-sector/%<%Y%m%d>---${slug}.org"
                                         "#+title: ${title}\n#+FILETAGS: :thel-sector: %^G\n\n")
                      :unnarrowed t)
       ))

;; A menu of common tasks for `org-roam'.
(defvar jnf/org-subject-menu--title (with-faicon "book" "Org Subject Menu" 1 -0.05))
(pretty-hydra-define jnf/org-subject-menu--default (:foreign-keys warn :title jnf/org-subject-menu--title :quit-key "q" :exit t)
  ("Projects"
   (
    ("h +" jnf/org-roam--hesburgh-libraries--capture     "Hesburgh Libraries: Capture Node…")
    ("h !" jnf/org-roam--hesburgh-libraries--node-insert "Hesburgh Libraries: Insert Node…")
    ("h ?" jnf/org-roam--hesburgh-libraries--node-find   " └─ Find Node…")
    ("t +" jnf/org-roam--thel-sector--capture     "Thel Sector: Capture Node…")
    ("t !" jnf/org-roam--thel-sector--node-insert "Thel Sector: Insert Node…")
    ("t ?" jnf/org-roam--thel-sector--node-find   " └─ Find Node…")
    )
   "Org Mode"
   (("+" org-roam-capture               "Capture Node…")
    ("!" org-roam-node-insert           "Insert Node…")
    ("?" org-roam-node-find             " └─ Find Node…")
    ("@" org-roam-dailies-capture-today "Capture Daily…")
    ("#" org-roam-buffer-toggle         "Toggle Org Roam Buffer"))))

(pretty-hydra-define jnf/org-subject-menu--thel-sector (:foreign-keys warn :title jnf/org-subject-menu--title :quit-key "q" :exit t)
  ("Thel Sector Subject Menu"
   (("+" jnf/org-roam--thel-sector--capture     "Thel Sector: Capture Node…")
    ("!" jnf/org-roam--thel-sector--node-insert "Thel Sector: Insert Node…")
    ("?" jnf/org-roam--thel-sector--node-find   " └─ Find Node…")
    ("@" org-roam-dailies-capture-today "Capture Daily…")
    ("#" org-roam-buffer-toggle         "Toggle Org Roam Buffer"))))

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
  :bind (("s-i" . jnf/org-subject-menu--default/body)
         ("C-c r" . jnf/org-subject-menu--default/body)))

(org-roam-setup)
(provide 'jnf-org-roam-v2.el)
;;; jnf-org-roam-v2.el ends here
