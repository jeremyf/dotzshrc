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
  :after pretty-hydra
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory "~/git/org")
  :bind (:map org-roam-mode-map
              (("C-c r l" . org-roam)
               ("C-c r f" . org-roam-find-file)
               ("C-c r i" . org-roam-insert)
               ("C-c r c" . org-roam-capture)
               ("C-c r x" . org-roam-jump-to-index)
               ("C-c r t" . jnf-toggle-roam-project-filter)
               ("C-c r g" . org-roam-graph))
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
  (setq org-roam-db-update-idle-seconds 2)
  (setq org-roam-db-update-method 'immediate)
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
          ("e" "Epigraph (Permanent > Epigraphs)" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "permanent/epigraphs/000-unsorted---${slug}"
           :head "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n#+begin_quote\n\n#+end_quote"
           :unnarrowed t)
          ("l" "Permanent > Letters" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "permanent/letters/%<%Y%m%d>---${slug}"
           :head "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n"
           :unnarrowed t)
          ("a" "Project > Ardu" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "projects/ardu/%<%Y%m%d>---${slug}"
           :head  "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n** Summary\n\n** Details\n\n** COMMENT For Referee"
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
          ("o" "Project > Distributed Autonomous Organization (DAO)" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "projects/distributed-autonomous-organization/%<%Y%m%d>---${slug}"
           :head  "#+title: ${title}\n#+roam_tags: dao\n* ${title}\n\n"
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
           :head  "#+title: ${title}\n#+roam_tags:\n* ${title}\n\n** Summary\n\n** Details\n\n** COMMENT For Referee"
           :unnarrowed t
           :immediate-finish t))))
(require 'org-roam-protocol)

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

(defun xah-filter-list (@predicate @sequence)
   "Return a new list such that @PREDICATE is true on all members of @SEQUENCE.
URL `http://ergoemacs.org/emacs/elisp_filter_list.html'
Version 2016-07-18"
   (delete
    "e3824ad41f2ec1ed"
    (mapcar
     (lambda ($x)
       (if (funcall @predicate $x)
           $x
         "e3824ad41f2ec1ed" ))
     @sequence)))

(defmacro org-roam-inserter-fn (project)
  "Define a function to wrap the `org-roam-inser' with a filter for the given PROJECT."
  (let* ((fn-name (intern (concat "org-roam-insert--filter-with--"
                                  (replace-regexp-in-string "\\W+" "-" project))))
         (docstring (concat "Insert an `org-roam' file for: " project)))
    `(defun ,fn-name (&optional lowercase completions description link-type)
       ,docstring
       (interactive "P")
       (let* ((filter (lambda(completions) (xah-filter-list
                                            (lambda(item) (string-match-p (concat "\\W" ,project "\\W") (first item)))
                                            completions))))
         (org-roam-insert lowercase completions filter description link-type)))))

(defmacro go-roam-find-file-project-fn (project)
  "Define a function to find an `org-roam' file within the given PROJECT."
  (let* ((fn-name (intern (concat "org-roam-find-file--" (replace-regexp-in-string "\\W+" "-" project))))
         (docstring (concat "Find an `org-roam' file for: " project)))
    `(defun ,fn-name (&optional initial-prompt completions)
       ,docstring
       (interactive "P")
       (let* ((filter (lambda(completions) (xah-filter-list
                                            (lambda(item) (progn (message (first item))(string-match-p (concat "\\W" ,project "\\W") (first item))))
                                            completions))))
         (org-roam-find-file initial-prompt completions filter)))))

(go-roam-find-file-project-fn "thel-sector")
(go-roam-find-file-project-fn "ardu")
(go-roam-find-file-project-fn "permanent,letters")
(go-roam-find-file-project-fn "permanent,bibliographies")
(go-roam-find-file-project-fn "permanent,cards")
(go-roam-find-file-project-fn "permanent,epigraphs")
(go-roam-find-file-project-fn "hesburgh-libraries")
(go-roam-find-file-project-fn "distributed-autonomous-organization")
(go-roam-find-file-project-fn "rpgs")
(go-roam-find-file-project-fn "samvera")
(org-roam-inserter-fn "thel-sector")
(org-roam-inserter-fn "ardu")
(org-roam-inserter-fn "permanent,bibliographies")
(org-roam-inserter-fn "permanent,cards")
(org-roam-inserter-fn "permanent,letters")
(org-roam-inserter-fn "permanent,epigraphs")
(org-roam-inserter-fn "hesburgh-libraries")
(org-roam-inserter-fn "distributed-autonomous-organization")
(org-roam-inserter-fn "rpgs")
(org-roam-inserter-fn "samvera")

(defvar jnf-org-subject-menu--title (with-faicon "book" "Org Subject Menu" 1 -0.05))
(pretty-hydra-define jnf-org-subject-menu (:foreign-keys warn :title jnf-org-subject-menu--title :quit-key "q")
  (
   "Permanent"
   (("b" org-roam-insert--filter-with--permanent-bibliographies "Bibliography")
    ("B" org-roam-find-file--permanent-bibliographies " └─ Find")
    ("c" org-roam-insert--filter-with--permanent-cards "Card")
    ("C" org-roam-find-file--permanent-cards " └─ Find")
    ("e" org-roam-insert--filter-with--permanent-epigraphs "Epigraph")
    ("E" org-roam-find-file--permanent-epigraphs " └─ Find")
    ("l" org-roam-insert--filter-with--permanent-letters "Letter")
    ("L" org-roam-find-file--permanent-letters " └─ Find"))
   "RPGs"
   (("a" org-roam-insert--filter-with--ardu "Ardu, World of")
    ("A" org-roam-find-file--ardu " └─ Find")
    ("r" org-roam-insert--filter-with--rpgs "RPGs (General)")
    ("R" org-roam-find-file--rpgs " └─ Find")
    ("t" org-roam-insert--filter-with--thel-sector "Thel Sector")
    ("T" org-roam-find-file--thel-sector " ├─ Find")
    ("2" org-roam-insert-random-thel-sector-npc  " └─ NPC"))
   "Work"
   (("d" org-roam-insert--filter-with--distributed-autonomous-organization "DAOs")
    ("D" org-roam-find-file--distributed-autonomous-organization " └─ Find")
    ("h" org-roam-insert--filter-with--hesburgh-libraries "Hesburgh Libraries")
    ("H" org-roam-find-file--hesburgh-libraries " └─ Find")
    ("s" org-roam-insert--filter-with--samvera "Samvera")
    ("S" org-roam-find-file--samvera " └─ Find"))
   "General Org"
   (("i" org-roam-insert "Insert Unfiltered")
    ("I" org-roam-find-file " └─ Find")
    ("O" gorg "Agenda")
    ("R" org-roam-jump-to-index "Roam Index"))
))

(global-set-key (kbd "s-1") 'jnf-org-subject-menu/body) ;; Deprecated
(global-set-key (kbd "s-i") 'jnf-org-subject-menu/body)

(defun jnf-toggle-roam-project-filter (project)
  "Prompt for a PROJECT, then toggle the `s-i' kbd to filter for that project."
  (interactive (list
                (completing-read
                 "Project: " '((":all" 1)
                               ("ardu" 2)
                               ("distributed-autonomous-organization" 3)
                               ("hesburgh-libraries" 4)
                               ("permanent-bibliographies" 5)
                               ("permanent-cards" 6)
                               ("rpgs" 7)
                               ("samvera" 8)
                               ("thel-sector" 9)))))
  (if (string= project ":all")
      (global-set-key (kbd "s-i") 'jnf-org-subject-menu/body)
    (global-set-key (kbd "s-i") (intern (concat "org-roam-insert--filter-with--" project)))))


(provide 'jnf-org-roam.el)
;;; jnf-org-roam.el ends here
