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
                     "\nlicenses:\n- by-nc-nd-4_0"
                     "\nslug: " slug
                     "\ntitle: " title
                     "\ntype: post"
                     "\n---\n")
                    nil (expand-file-name fpath) nil nil nil t)
      (find-file (expand-file-name fpath))
      )))


;; Used in ./emacs/snippets/markdown-mode/tag
(defun tor-tags-list ()
  "Return a list of tags from TakeOnRules.com."
  (with-temp-buffer
    (insert-file-contents (concat tor--repository-path "/artifacts/tags.txt"))
    (split-string (buffer-string) "\n" t)))

(provide 'jnf-blogging.el)
;;; jnf--blogging.el ends here
