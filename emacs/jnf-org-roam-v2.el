;;; jnf-org-roam-v2.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  This package provides configuration for org-roam
;;
;;; Code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN org-roam overrides that were very much a copy, paste, and then replace.
(cl-defun jnf/org-roam-capture (&optional goto keys &key templates filter-fn)
  "Launches an `org-capture' process for a new or existing note.

This uses the templates defined at `org-roam-capture-templates'.
Arguments GOTO and KEYS see `org-capture'.
Argument TEMPLATES see `org-roam-capture-'.
Argument FILTER-FN see `org-roam-node-read'.

NOTE: This is a copy of the code from the original `org-roam-capture'."
  (interactive "P")
  (let ((node (org-roam-node-read nil filter-fn)))
    (org-roam-capture- :goto goto
                       :node node
                       :templates templates
                       :props '(:immediate-finish nil))))

(cl-defun jnf/org-roam-node-insert (&optional filter-fn &key templates)
  "Find an Org-roam node and insert (where the point is) an \"id:\" link to it.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.

Argument TEMPLATES see `org-roam-capture-'.

NOTE: This is a copy of the code from the original `org-roam-node-insert'."
  (interactive)
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
               (node (org-roam-node-read region-text filter-fn))
               (description (or region-text
                                (org-roam-node-title node))))
          (if (org-roam-node-id node)
              (progn
                (when region-text
                  (delete-region beg end)
                  (set-marker beg nil)
                  (set-marker end nil))
                (insert (org-link-make-string
                         (concat "id:" (org-roam-node-id node))
                         description)))
            (org-roam-capture-
             :node node
             :templates templates
             :props (append
                     (when (and beg end)
                       (list :region (cons beg end)))
                     (list :insert-at (point-marker)
                           :link-description description
                           :finalize 'insert-link))))))
    (deactivate-mark)))

(cl-defun jnf/org-roam-node-find (&optional other-window initial-input filter-fn &key templates)
  "Find and open an Org-roam node by its title or alias.

INITIAL-INPUT is the initial input for the prompt.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
If OTHER-WINDOW, visit the NODE in another window.
Argument TEMPLATES see `org-roam-capture-'.

NOTE: This is a copy of the code from the original `org-roam-node-insert'."
  (interactive current-prefix-arg)
  (let ((node (org-roam-node-read initial-input filter-fn)))
    (if (org-roam-node-file node)
        (org-roam-node-visit node other-window)
      (org-roam-capture-
       :node node
       :templates templates
       :props '(:finalize find-file)))))
;; END org-roam overrides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
       (jnf/org-roam-capture goto
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
         (jnf/org-roam-node-insert (lambda (node) (-contains-p (org-roam-node-tags node) ,project))
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
         (jnf/org-roam-node-find other-window
                                 initial-input
                                 (lambda (node) (-contains-p (org-roam-node-tags node) ,project))
                                 :templates (jnf/org-roam-templates-for ,project-as-symbol)))))

(create-org-roam-capture-fn-for "thel-sector")
(create-org-roam-node-insert-fn-for "thel-sector")
(create-org-roam-node-find-fn-for "thel-sector")

;; A Property List of my `org-roam' capture templates.
(setq jnf/org-roam-capture-templates-plist
      (list
       :thel-sector '("t" "Thel Sector" plain "%?"
                      :if-new (file+head "personal/thel-sector/%<%Y%m%d>---${slug}.org"
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

;; A menu of common tasks for `org-roam'.
(defvar jnf/org-subject-menu--title (with-faicon "book" "Org Subject Menu" 1 -0.05))
(pretty-hydra-define jnf/org-subject-menu (:foreign-keys warn :title jnf/org-subject-menu--title :quit-key "q" :exit t)
  ("Projects"
   (("t +" jnf/org-roam--thel-sector--capture     "Thel Sector: Capture Node…")
    ("t !" jnf/org-roam--thel-sector--node-insert "Thel Sector: Insert Node…")
    ("t ?" jnf/org-roam--thel-sector--node-find   " └─ Find Node…"))
   "Org Mode"
   (("+" jnf/org-roam-capture           "Capture Node…")
    ("@" org-roam-dailies-capture-today " └─ Daily…")
    ("!" jnf/org-roam-node-insert       "Insert Node…")
    ("?" jnf/org-roam-node-find         " └─ Find Node…")
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
  :bind (("s-i" . jnf/org-subject-menu/body)
         ("C-c r" . jnf/org-subject-menu/body)))

(org-roam-setup)
(provide 'jnf-org-roam-v2.el)
;;; jnf-org-roam-v2.el ends here
