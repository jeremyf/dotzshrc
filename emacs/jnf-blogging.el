;;; jnf-blogging.el --- Summary
;;
;;; Commentary:
;;
;;  This package provides some blogging tooling.
;;
;;  Todo:
;;
;;  - [X] tor-post-new :: a function for creating a new post
;;  - [ ] tor-tags :: a function for prompting for existing tags
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO - Complete this function.
;; - [X] Prompt for and build proper filename
;; - [X] Create named file
;; - [X] Populate named file with pre-amble
(defun tor-post-new (title &optional)
  "Create and visit a new draft blog post for the prompted TITLE.

The file for the blog post conforms to the path schema of posts for TakeOnRules.com."
  (interactive (list (read-from-minibuffer
		      "Title: "
		      nil nil nil nil)))

  (let ((default-directory (concat "~/git/takeonrules.github.io/content/posts/"
                                   (format-time-string "%Y/"))))
    (let ((fpath (concat default-directory (s-dashed-words title) ".md"))
          (slug (s-dashed-words title)))
      (write-region (concat
                     "---"
                     "\ndate: " (format-time-string "%Y-%m-%d %H:%M:%S %Z")
                     "\ndraft: true"
                     "\nlicenses:\n- by-nc-nd-4_0"
                     "\nslug: " slug
                     "\ntitle: " title
                     "\ntype: post"
                     "\n---\n")
                    nil (expand-file-name fpath) nil nil nil t)
      (find-file (expand-file-name fpath))
      )))
(provide 'jnf-blogging.el)
;;; jnf--blogging.el ends here
