;;; -*- lexical-binding: t; -*-
;;; jnf-blogging.el --- Summary
;;
;;; Commentary:
;;
;;  This package provides some blogging tooling.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/jrblevin/markdown-mode/
(use-package markdown-mode
  :straight t
  ;; :hook ((markdown-mode . turn-on-visual-line-mode))
  :bind (:map markdown-mode-map ("C-c t" . jnf/tor-subject-menu/body))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/pandoc"))

(defvar jnf/tor-menu--title (with-faicon "pencil-square" "Take on Rules" 1 -0.05))
(pretty-hydra-define jnf/tor-subject-menu (:foreign-keys warn :title jnf/tor-menu--title :quit-key "q")
  ("Posts"
   (("c" tor-cite-active-region-dwim "Cite region…")
    ("n" tor-post-new "New post…")
    ("m" tor-wrap-as-marginnote-dwim "Margin note region…")
    ("r" jnf/retitle-tor-content "Re-title content…")
    ("t" tor-tag-post "Tag post…"))))

(defun tor-post-new (title)
  "Create and visit a new draft post.  Prompt for a `TITLE'.

The file for the blog post conforms to the path schema of posts
for TakeOnRules.com."
  (interactive "sTitle: ")
  (tor-post---create-or-append title))

(defun tor-tag-post (tag)
  "Apply the TAG to the current TakeOnRules.com post.

No effort is made to check if this is a post."
  (interactive (list (completing-read "Tag: " (tor-tags-list))))
  (let ((saved-point (point))
        (to-insert (concat "\n- " tag)))
    (replace-regexp "^tags:$" (concat "tags:" to-insert) nil 0 (point-max))
    (goto-char (+ saved-point (length to-insert)))))

(defun tor-wrap-as-marginnote-dwim ()
  "Warp between line or current region a marginnote.

TODO: If no region, wrap current line."
  (interactive)
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (crux-get-positions-of-line-or-region)))

    (goto-char end)
    (insert "\n{{< /marginnote >}}")
    (goto-char beg)
    (insert "{{< marginnote >}}\n")))

(defun tor-cite-active-region-dwim (url)
  "Wrap current region (or point) in a `CITE' and optional `A' tag with URL.

For the URL:

- If `car' in `kill-ring' starts with \"http\", then use that as the URL.
- Otherwise prompt for a URL.

If the URL an empty string, then wrap the current region or point
in a CITE tag. Else, if we have a non-0 length URL, wrap it in
CITE and A tag."
  (interactive (list
                (if (string= (substring
                              (substring-no-properties (car kill-ring)) 0 4) "http")
                    (substring-no-properties (car kill-ring))
                  (read-string "URL (optional): "))))

  ;; Were we to start writing at the START position, we'd invariably
  ;; change the contents such that the END position was no longer
  ;; accurate.  So instead, we append at the END position, hop back to
  ;; the START position and append to the START position.
  (let* ((start (if (use-region-p) (region-beginning) (point)))
         (end (if (use-region-p) (region-end) (point))))
    (if (eq (length url) 0)
        (progn
          (goto-char end)
          (insert "</cite>")
          (goto-char start)
          (insert (concat
                   "<cite>")))
      (progn
        (goto-char end)
        (insert "</a></cite>")
        (goto-char start)
        (insert (concat
                 "<cite><a href=\""
                 url
                 "\" class=\"u-url p-name\" rel=\"cite\">"))))))

(defun tor-sync ()
  "Synchronize TakeOnRules.com repositories."
  (interactive)
  (message "Synchronizing TakeOnRules.com local git repos...")
  (dolist
      (path '(
              ;; The themes directory
              "~/git/takeonrules.github.io/themes/hugo-tufte"
              ;; The content directory
              "~/git/takeonrules.github.io/"
              ;; The tooling directory
              "~/git/dotzshrc"
              ;; The background knowledge directory
              "~/git/org"))
    (message (concat "Syncing \"" path "\"..."))
    (shell-command-to-string
     (concat
      "cd " path
      " && git pull --rebase"
      " && git push -u --force-with-lease")))
  (message "Finished synchronizing TakeOnRules.com local git repos."))

(global-set-key (kbd "s-7") 'tor-post-amplifying-the-blogosphere)
(global-set-key (kbd "<f7>") 'tor-post-amplifying-the-blogosphere)

(cl-defun tor-post-amplifying-the-blogosphere (subheading &key citeTitle citeURL citeAuthor)
  "Create and visit draft post for amplifying the blogosphere.

If there's an active region, prompt for the `SUBHEADING'.  The file
for the blog post conforms to the path schema of posts for
TakeOnRules.com.

We'll pass the `CITETITLE', `CITEAUTHOR', and `CITEURL' to
`tor-post---create-or-append'"
  (interactive (list (if (use-region-p)
                         (read-string "Sub-Heading: ")
                       nil)))
  (tor-post---create-or-append
   (format-time-string "Amplifying the Blogosphere (v%Y-%m-%d)")
   :toc "true"
   :subheading subheading
   :series "amplifying-the-blogosphere"
   :tags "response to other blogs"
   :citeTitle citeTitle
   :citeURL citeURL
   :citeAuthor citeAuthor))

(cl-defun tor-post---create-or-append (title &key tags series toc citeTitle citeURL citeAuthor subheading)
  "Create or append a post with `TITLE'.

The following keys are optional:

`TAGS' one or more tags, as a list or string, to add to the
        frontmatter.
`SERIES' the series to set in the frontmatter.
`TOC' whether to include a table of contents in the post.
`CITETITLE' the title of the URL cited (if any)
`CITEURL' the URL cited (if any)
`CITEAUTHOR' the author cited (if any)
`SUBHEADING' if you have an active region, use this header.

If there's an active region, select that text and place it."
  (let* ((default-directory (concat "~/git/takeonrules.github.io/"
                                    "/content/posts/"
                                    (format-time-string "%Y/")))
         (slug (s-dashed-words title))
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
                 ("\ntags:")
                 (if tags (concat (mapconcat
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
                       "\n{{< blockquote"
                       (if citeAuthor
                           (concat " pre=\"" citeAuthor "\""))
                       " cite=\""
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

(cl-defun tor-list-by-key-from-filename (&key key filename)
  "Build a list of entries of the `KEY' from the `FILENAME'."
  (split-string-and-unquote
   (shell-command-to-string
    (concat
     "rg \"" key ": .*$\" "
     (f-join "~/git/takeonrules.github.io/" filename)
     " --only-matching --no-filename | cut -d \" \" -f 2- | sort | tr '\n' '~'"))
   "~"))

;; Used in ./emacs/snippets/text-mode/tag
(defun tor-tags-list ()
  "Return a list of tags from TakeOnRules.com."
  (tor-list-by-key-from-filename :key "tag" :filename "data/glossary.yml"))

(defun tor-game-list ()
  "Return a list of games from TakeOnRules.com."
  (tor-list-by-key-from-filename :key "game" :filename "data/glossary.yml"))

(defun tor-glossary-title-list ()
  "Return a list of titles from TakeOnRules.com."
  (tor-list-by-key-from-filename :key "title" :filename "data/glossary.yml"))

(defun tor-glossary-key-list ()
  "Return a list of keys from TakeOnRules.com glossary."
  (tor-list-by-key-from-filename :key "key" :filename "data/glossary.yml"))

(defun tor-series-list ()
  "Return a list of series from TakeOnRules.com."
  (tor-list-by-key-from-filename :key "key" :filename "data/series.yml"))

(defun tor-licenses-list ()
  "Return a list of available licenses for TakeOnRules.com."
    (tor-list-by-key-from-filename :key "Key" :filename "data/licenses.yml"))

(defun tor-page-relative-pathname-list ()
  "Return a list of pages for TakeOnRules.com."
  (split-string-and-unquote
   (let ((default-directory "~/git/takeonrules.github.io/content"))
     (shell-command-to-string "rg \"^title: \" --files-with-matches | sort"))))

(defun tor-asset-relative-pathname-list ()
  "Return a list of image filenames for TakeOnRules.com."
  (split-string-and-unquote
   (let ((default-directory "~/git/takeonrules.github.io/assets/images"))
     (shell-command-to-string "ls"))))

(defun org-files-names-in-project-list ()
  "Return a list of filenames in the current files directory."
  (split-string-and-unquote
   (shell-command-to-string
    (concat
     "ls " (file-name-directory buffer-file-name)))))

(defun jnf/roll (sided)
  "Roll an n `SIDED' die."
  (interactive "sSides: ")
  (let ((result (+ 1 (random (cl-parse-integer sided)))))
    (message "d%s => %s" sided result)))

(defun jnf/roll-expression (expression)
  "Roll the `EXPRESSION'."
  (interactive "sExpression: ")
  (-let* (((rolls . result) (org-d20--roll expression)))
    (message "%s => %s" expression result)))
(global-set-key (kbd "C-s-r") 'jnf/roll-expression)

(defun jnf/retitle-tor-content (title)
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
