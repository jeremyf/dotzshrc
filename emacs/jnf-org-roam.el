;;; -*- lexical-binding: t; -*-
;;; jnf-org-roam.el --- Summary
;;
;;; Commentary:
;;
;;  This package provides configuration for org-roam
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN ORG ROAM  and concerns
;;
;; With the latest update of org-roam, things again behavior
;; correctly.  Now I can just load org-roam as part of my day to day
(use-package org-roam
  :straight t
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory "~/git/org")
  :bind (:map org-roam-mode-map
              (("C-c r l" . org-roam)
               ("C-c r f" . org-roam-find-file)
               ("C-c r i" . org-roam-insert)
               ("C-c r c" . org-roam-capture)
               ("C-c r x" . org-roam-jump-to-index)
               ("C-c r g" . org-roam-graph-show))
              :map org-mode-map
              (("C-c r c" . org-roam-capture))
              (("C-c r x" . org-roam-jump-to-index)))
  ;; Use the traditional org first "*" element then the "#+title:"
  ;; property to establish the title of the object, then append the
  ;; "#+roam_alias:" as alternate titles
  ;;
  :init (setq org-roam-tag-sources '(prop all-directories))
  (setq org-roam-index-file "~/git/org/index.org")
  ;; First use "#+roam_tags:" then the directory structure to build the
  ;; list of tags.
  ;;
  ;; See https://www.orgroam.com/manual/Tags.html#Tags
  ;;
  (setq org-roam-title-sources '((title headline) alias))
  (setq org-roam-update-db-idle-seconds 300)
  ;; Note: Order of these templates matters. The `org-roam-insert-immediate` uses
  ;; the first one in the list (e.g. Fleeting)
  ;;
  (setq org-roam-capture-templates
        '(
          ("f" "Fleeting" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "fleeting/%<%Y%m%d>---${slug}"
           :head "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n"
           :unnarrowed t)
          ("b" "Permanent > Bibliographic" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "permanent/bibliographies/%<%Y%m%d>---${slug}"
           :head "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n"
           :unnarrowed t)
          ("c" "Permanent > Card (Unsorted)" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "permanent/cards/000-unsorted---${slug}"
           :head "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n"
           :unnarrowed t)
          ("l" "Permanent > Letters" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "permanent/letters/%<%Y%m%d>---${slug}"
           :head "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n"
           :unnarrowed t)
          ("a" "Project > Ardu" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "projects/ardu/%<%Y%m%d>---${slug}"
           :head  "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n"
           :unnarrowed t
           :immediate-finish t)
          ("d" "Project > Diversity Equity Incluson (DEI)" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "projects/diversity-equity-inclusion/%<%Y%m%d>---${slug}"
           :head  "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n"
           :unnarrowed t
           :immediate-finish t)
          ("h" "Project > Hesburgh Libraries" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "projects/hesburgh-libraries/%<%Y%m%d>---${slug}"
           :head  "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n"
           :unnarrowed t
           :immediate-finish t)
          ("r" "Project > RPGs" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "projects/rpgs/%<%Y%m%d>---${slug}"
           :head  "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n"
           :unnarrowed t
           :immediate-finish t)
          ("s" "Project > Samvera" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "projects/samvera/%<%Y%m%d>---${slug}"
           :head  "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n"
           :unnarrowed t
           :immediate-finish t)
          ("t" "Project > Thel Sector" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "projects/thel-sector/%<%Y%m%d>---${slug}"
           :head  "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n"
           :unnarrowed t
           :immediate-finish t))))
(require 'org-roam-protocol)

(defmacro go-roam-find-file-project-fn (project)
  "Define a function to find an `org-roam' file within the given PROJECT."
  (let* ((fn-name (intern (concat "go-roam-find-" (replace-regexp-in-string " +" "-" project))))
         (docstring (concat "Find an `org-roam' file for: " project)))
    `(defun ,fn-name (&optional completions filter-nf no-confirm)
       ,docstring
       (interactive)
       (org-roam-find-file (concat ,project " ") completions filter-nf no-confirm))))

(go-roam-find-file-project-fn "thel-sector")
(go-roam-find-file-project-fn "ardu")
(go-roam-find-file-project-fn "permanent bibliographies")
(go-roam-find-file-project-fn "permanent cards")
(go-roam-find-file-project-fn "hesburgh libraries")
(go-roam-find-file-project-fn "samvera")
(defvar jnf-find-file-in-roam-project--title (all-the-icons-faicon "book" "Find File in Roam Project" 1 -0.05))
(pretty-hydra-define jnf-find-file-in-roam-project (:foreign-keys warn :title jnf-find-file-in-roam-project--title :quit-key "q")
  (
   "Permanent"
   (("b" go-roam-find-permanent-bibliographies "Bibliography")
    ("c" go-roam-find-permanent-cards "Card"))
   "RPGs"
   (("a" go-roam-find-ardu "Ardu, World of")
    ("t" go-roam-find-thel-sector "Thel Sector"))
   "Work"
   (("h" go-roam-find-permanent-hesburgh-libraries "Hesburgh Libraries")
    ("s" go-roam-find-permanent-samvera "Samvera"))

   ))
(global-set-key (kbd "s-1") 'jnf-find-file-in-roam-project/body)

(use-package org-roam-server
  :straight t
  :after org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

;; https://ag91.github.io/blog/2020/11/12/write-org-roam-notes-via-elisp/
(defun jnf/make-filepath (title now &optional zone)
  "Make filename from note TITLE and NOW time (assumed in the current time ZONE)."
  (f-join
   org-roam-directory
   "fleeting"
   (concat
    (format-time-string "%Y%m%d---" now (or zone (current-time-zone)))
    (org-roam--title-to-slug title)
    ".org")))

(defun jnf/insert-org-roam-file (file-path title &optional links sources text quote)
  "Insert org roam file in FILE-PATH with TITLE, LINKS, SOURCES, TEXT, QUOTE."
  (with-temp-file file-path
    (insert
     "#+TITLE: " title
     "\n\n- tags :: " (--reduce (concat acc ", " it) links) "\n"
     (if sources (concat "- source :: " (--reduce (concat acc ", " it) sources) "\n") "")
     "\n"
     (if text text "")
     "\n"
     "\n"
     (if quote
         (concat "#+begin_src text \n"
     quote "\n"
     "#+end_src")
       ""))))

(defun send-roaming ()
  "Convert an org node to a `org-roam' note."
  (interactive)
  (let* ((heading (org-get-heading t t t t))
         (body (org-get-entry))
         (link (format "[[id:%s][%s]]" (org-id-get-create) heading))
         (filepath (jnf/make-filepath heading (current-time))))
    (jnf/insert-org-roam-file
     filepath
     heading
     nil
     (list link)
     (format "* Note stored from tasks\n%s" body)
     nil)
    (find-file filepath)))

;; Some hot keys to jump to often different relevant files
;; Jump to the current clock if one is open, otherwise, go to my agenda file.
(global-set-key (kbd "s-i") 'org-roam-insert)
(provide 'jnf-org-roam.el)
;;; jnf-org-roam.el ends here
