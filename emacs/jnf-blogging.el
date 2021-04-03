;;; -*- lexical-binding: t; -*-
;;; jnf-blogging.el --- Summary
;;
;;; Commentary:
;;
;;  This package provides some blogging tooling.
;;
;;  Todo:
;;
;;  - [X] tor-post-new :: a function for creating a new post
;;  - [X] tor-tags-list :: a function for prompting for existing tags
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tor--repository-path "~/git/takeonrules.github.io/")

;; TODO - Complete this function.
;; - [X] Prompt for and build proper filename
;; - [X] Create named file
;; - [X] Populate named file with pre-amble
(defun tor-post-new (title &optional)
  "Create and visit a new draft blog post for the prompted TITLE.

The file for the blog post conforms to the path schema of posts
for TakeOnRules.com."
  (interactive (list (read-from-minibuffer
		      "Title: "
		      nil nil nil nil)))

  (let ((default-directory (concat tor--repository-path
                                   "/content/posts/"
                                   (format-time-string "%Y/"))))
    (let ((fpath (concat default-directory (s-dashed-words title) ".md"))
          (slug (s-dashed-words title)))
      (write-region (concat
                     "---"
                     "\ndate: " (format-time-string "%Y-%m-%d %H:%M:%S %z")
                     "\ndraft: true"
                     "\nhive:"
                     "\n  url:"
                     "\n  tags: hive-150329 hive-110490 hive-177956 hive-177682"
                     "\n  postDate: " (format-time-string "%Y-%m-%d")
                     "\nlayout: post"
                     "\nlicenses:\n- by-nc-nd-4_0"
                     "\nslug: " slug
                     "\ntitle: '" title "'"
                     "\ntype: post"
                     "\n---\n")
                    nil (expand-file-name fpath) nil nil nil t)
      (find-file (expand-file-name fpath))
      )))

;; Used in ./emacs/snippets/text-mode/tag
(defun tor-tags-list ()
  "Return a list of tags from TakeOnRules.com."
  (split-string-and-unquote
   (shell-command-to-string
    (concat
     "ag \"tag: .*$\" "
     (f-join tor--repository-path "data/glossary.yml")
     " -o --nofilename | cut -d \" \" -f 2- | sort | tr '\n' '~'"))
   "~"))

(defun tor-abbrs-list ()
  "Return a list of abbrs from TakeOnRules.com."
  (split-string-and-unquote
   (shell-command-to-string
    (concat
     "ag \"key: .*$\" "
     (f-join tor--repository-path "data/glossary.yml")
     " -o --nofilename | cut -d \" \" -f 2- | sort | tr '\n' '~'"))
   "~"))

(defun tor-game-list ()
  "Return a list of games from TakeOnRules.com."
  (split-string-and-unquote
   (shell-command-to-string
    (concat
     "ag \"game: .*$\" "
     (f-join tor--repository-path "data/glossary.yml")
     " -o --nofilename | cut -d \" \" -f 2- | sort | tr '\n' '~'"))
   "~"))

(defun tor-glossary-title-list ()
  "Return a list of titles from TakeOnRules.com."
  (split-string-and-unquote
   (shell-command-to-string
    (concat
     "ag \"title: .*$\" "
     (f-join tor--repository-path "data/glossary.yml")
     " -o --nofilename | cut -d \" \" -f 2- | sort | tr '\n' '~'"))
   "~"))

;; Used in ./emacs/snippets/text-mode/series
(defun tor-series-list ()
  "Return a list of series from TakeOnRules.com."
  (split-string-and-unquote
   (shell-command-to-string (concat
                            "ag \"key: .*$\" "
                            (f-join tor--repository-path "data/series.yml")
                            " -o --nofilename | cut -d \" \" -f 2- | sort"))))

(defun tor-licenses-list ()
  "Return a list of available licenses for TakeOnRules.com."
  (split-string-and-unquote
   (shell-command-to-string
    (concat
     "ag \"Key: .*$\" "
     (f-join tor--repository-path "data/licenses.yml")
     " -o --nofilename | cut -d \" \" -f 2- | sort | tr '\n' '~'"))
   "~"))

(defun tor-page-relative-pathname-list ()
  "Return a list of pages for TakeOnRules.com."
  (split-string-and-unquote
   (let ((default-directory (f-join tor--repository-path "content")))
     (shell-command-to-string (concat
                               "ag \"^title: \" --files-with-matches | sort"
                               )))))

(defun tor-asset-relative-pathname-list ()
  "Return a list of image filenames for TakeOnRules.com."
  (split-string-and-unquote
   (let ((default-directory (f-join tor--repository-path "assets/images")))
     (shell-command-to-string "ls"))))

(defun org-files-names-in-project-list ()
  "Return a list of filenames in the current files directory."
  (split-string-and-unquote
   (shell-command-to-string
    (concat
     "ls " (file-name-directory buffer-file-name)))))

(provide 'jnf-blogging.el)
;;; jnf-blogging.el ends here
