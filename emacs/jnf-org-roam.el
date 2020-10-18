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


;; (use-package company-org-roam
;;   :after company
;;   :straight (:host github :repo "org-roam/company-org-roam")
;;   :config (push 'company-org-roam company-backends))
;; (add-hook 'after-init-hook 'global-company-mode)
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

;; Some hot keys to jump to often different relevant files
;; Jump to the current clock if one is open, otherwise, go to my agenda file.
(global-set-key (kbd "<f3>") 'org-roam-jump-to-index)
(global-set-key (kbd "<f4>") `(lambda () (interactive)(find-file "~/git/org/permanent/card_index.org")))
(global-set-key (kbd "<f6>") `(lambda () (interactive)(find-file "~/git/org/permanent/bibliographic_index.org")))
(global-set-key (kbd "s-i") 'org-roam-find-file)
(provide 'jnf-org-roam.el)
;;; jnf-org-roam.el ends here
