;;; jnf-stars-without-number.el --- Summary
;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Some useful tools for Stars without Number
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun thel-sector-filename (title)
  "Convert the given TITLE to a filename."
  (f-join org-directory
          "projects"
          "thel-sector"
          (concat (format-time-string "%Y%m%d---") (s-snake-case title) ".org")))

(defun swn-npc (culture &optional)
  "Capture a random `swnt' npc of the prompted CULTURE.

This function writes to the as-of-now hard-coded Thel Sector
project in my org-directory."
  (interactive (list (completing-read "Culture: " '(
                                                    ("Arabic" 1)
                                                    ("Chinese" 2)
                                                    ("English" 3)
                                                    ("Greek" 4)
                                                    ("Indian" 5)
                                                    ("Japanese" 6)
                                                    ("Latin" 7)
                                                    ("Nigerian" 8)
                                                    ("Russian" 9)
                                                    ("Spanish" 10)))))
 (let* ((npc-string-list (split-string-and-unquote (shell-command-to-string (concat "swnt new npc --is-patron --culture " culture)) "\n"))
         (npc-name (string-trim (replace-regexp-in-string ":" "" (car npc-string-list))))
         (body (concat "#+title: " npc-name "\n\n* " npc-name "\n\n" (string-join npc-string-list "\n")))
         ;; Conforms to the capture template
         (fpath (thel-sector-filename npc-name)))
      (write-region body nil fpath nil nil nil t)
      (insert (concat "[[file:" fpath "][" npc-name "]]"))
      (org-roam-db-build-cache)))

(define-key org-mode-map (kbd "C-c s n") 'swn-npc)

(provide 'jnf-stars-without-number.el)
;;; jnf--stars-without-number.el ends here