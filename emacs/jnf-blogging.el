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

(defun tor-post-new (title &optional)
  "Create and visit a new draft post.  Prompt for a `TITLE'.

The file for the blog post conforms to the path schema of posts
for TakeOnRules.com."
  (interactive "sTitle: ")
  (tor-post---create-or-append title))

(global-set-key (kbd "s-7") 'tor-post-amplifying-the-blogosphere)
(global-set-key (kbd "<f7>") 'tor-post-amplifying-the-blogosphere)

(defun tor-post-amplifying-the-blogosphere (subheading &rest ARGS)
  "Create and visit draft post for amplifying the blogosphere.

If there's an active region, prompt for the `SUBHEADING'.  The file
for the blog post conforms to the path schema of posts for
TakeOnRules.com.

Pull the `citeTitle' and `citeURL' from `ARGS' and pass those
along to the `tor-post---create-or-append'"
  (interactive (list (if (use-region-p)
                         (read-string "Sub-Heading: ")
                       nil)))
  (tor-post---create-or-append
   (format-time-string "Amplifying the Blogosphere (v%Y-%m-%d)")
   :toc "true"
   :subheading subheading
   :series "amplifying-the-blogosphere"
   :tags "response to other blogs"
   :citeTitle (plist-get ARGS :citeTitle)
   :citeURL (plist-get ARGS :citeURL)))

(defun tor-post---create-or-append (title &rest ARGS)
  "Create or append a post with `TITLE'.

The following `ARGS' are optional:

`:tags' one or more tags, as a list or string, to add to the
        frontmatter.
`:series' the series to set in the frontmatter.
`:toc' whether to include a table of contents in the post.
`:citeTitle' the title of the URL cited (if any)
`:citeURL' the URL cited (if any)
`:subheading' if you have an active region, use this header.

If there's an active region, select that text and place it."
  (let* ((default-directory (concat tor--repository-path
                                    "/content/posts/"
                                    (format-time-string "%Y/")))
         (slug (s-dashed-words title))
         (series (plist-get ARGS :series))
         (citeTitle (plist-get ARGS :citeTitle))
         (citeURL (plist-get ARGS :citeURL))
         (tags (plist-get ARGS :tags))
         (toc (plist-get ARGS :toc))
         (subheading (plist-get ARGS :subheading))
         (fpath (expand-file-name
                 (concat default-directory slug ".md"))))
    ;; If the file does not exist, create the file with the proper
    ;; frontmatter.
    (if (not (file-exists-p fpath))
        (write-region
         (concat "---"
                 "\ndate: " (format-time-string "%Y-%m-%d %H:%M:%S %z")
                 "\ndraft: true"
                 "\nlayout: post"
                 "\nlicenses:\n- all-rights-reserved"
                 "\nslug: " (format "%s" slug)
                 "\ntitle: '" title "'"
                 "\ntype: post"
                 (if series (concat "\nseries: " series))
                 (if toc (concat "\ntoc: true"))
                 (if tags (concat "\ntags:"
                                  (mapconcat
                                   (lambda (tag)
                                     (concat "\n- " tag))
                                   (flatten-tree tags) "")))
                 "\n---\n")
         nil fpath))
    ;; If we have an active region, append that region's content to
    ;; the given file.
    (if (use-region-p)
        (write-region
         (concat
          (if subheading
              (concat "\n## " subheading "\n")
            (if citeTitle (concat "\n## " citeTitle "\n")))
          (if citeURL (concat
                       "\n{{< blockquote cite=\""
                       citeTitle "\" cite_url=\""
                       citeURL "\" >}}\n"))
          (buffer-substring (region-beginning) (region-end))
          (if citeURL "\n{{< /blockquote >}}"))
         nil fpath t)
      ;; Without an active region, if we have a citeURL insert a link
      ;; to it.
      (if citeURL
          (write-region
           (concat
            "\n<cite><a href=\"" citeURL
            "\" class=\"u-url p-name\" rel=\"cite\">"
            (or (citeTitle) (citeURL)) "</a></cite>\n")
           nil fpath t)))
    ;; Finally open that file for editing.
    (find-file fpath)))

(defun tor-list-by-key-from-filename (key filename)
  "Build a list of entries of the `KEY' from the `FILENAME'."
  (split-string-and-unquote
   (shell-command-to-string
    (concat
     "rg \"" key ": .*$\" "
     (f-join tor--repository-path filename)
     " --only-matching --no-filename | cut -d \" \" -f 2- | sort | tr '\n' '~'"))
   "~"))

;; Used in ./emacs/snippets/text-mode/tag
(defun tor-tags-list ()
  "Return a list of tags from TakeOnRules.com."
  (tor-list-by-key-from-filename "tag" "data/glossary.yml"))

(defun tor-game-list ()
  "Return a list of games from TakeOnRules.com."
  (tor-list-by-key-from-filename "game" "data/glossary.yml"))

(defun tor-glossary-title-list ()
  "Return a list of titles from TakeOnRules.com."
  (tor-list-by-key-from-filename "title" "data/glossary.yml"))

(defun tor-glossary-key-list ()
  "Return a list of keys from TakeOnRules.com glossary."
  (tor-list-by-key-from-filename "key" "data/glossary.yml"))

(defun tor-series-list ()
  "Return a list of series from TakeOnRules.com."
  (tor-list-by-key-from-filename "key" "data/series.yml"))

(defun tor-licenses-list ()
  "Return a list of available licenses for TakeOnRules.com."
    (tor-list-by-key-from-filename "Key" "data/licenses.yml"))

(defun tor-page-relative-pathname-list ()
  "Return a list of pages for TakeOnRules.com."
  (split-string-and-unquote
   (let ((default-directory (f-join tor--repository-path "content")))
     (shell-command-to-string "rg \"^title: \" --files-with-matches | sort"))))

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

(defun jnf/roll (&optional sided)
  "Roll an n `SIDED' die."
  (interactive "sSides: ")
  (let ((result (+ 1 (random (cl-parse-integer sided)))))
    (message "d%s => %s" sided result)))

(defun jnf/roll-expression (&optional expression)
  "Roll the `EXPRESSION'."
  (interactive "sExpression: ")
  (-let* (((rolls . result) (org-d20--roll expression)))
    (message "%s => %s" expression result)))
(global-set-key (kbd "C-s-r") 'jnf/roll-expression)

(defun jnf/retitle-tor-content (&optional title)
  "Replace the given buffer's title with the new `TITLE'.

This function will: replace the content's title, update the slug,
and rename the buffer."
    (interactive "sTitle: ")
    (let* ((metadataTitle (concat "title: '" title "'"))
           (slug (s-dashed-words title))
           (metadataSlug (concat "slug: " slug))
           (filename (buffer-file-name))
           (new-filename (concat (file-name-directory filename)
                                 slug
                                 ".md")))

      ;; Replace the title metadata entry
      (goto-char (point-min))
      (while (search-forward-regexp "^title:.*$" nil t)
        (replace-match metadataTitle))

      ;; Replace the slug metadata entry
      (goto-char (point-min))
      (while (search-forward-regexp "^slug:.*$" nil t)
        (replace-match metadataSlug))


      ;; Need to save before we rename the buffer
      (save-buffer)

      ;; Rename the buffer, accounting for version control
      (cond
       ((vc-backend filename)
        (vc-rename-file filename new-filename))
         (t
          (rename-file filename new-filename t)
          (set-visited-file-name new-filename t t)))

      ;; Report filename change
      (message "Renamed %s -> %s" filename new-filename)))

(provide 'jnf-blogging.el)
;;; jnf-blogging.el ends here
