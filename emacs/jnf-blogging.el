;;; jnf-blogging.el --- Summary
;;
;;; Commentary:
;;
;;  This package provides some blogging tooling.
;;
;;  Todo:
;;
;;  - [ ] tor-post-new :: a function for creating a new post
;;  - [ ] tor-tags :: a function for prompting for existing tags
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO - Complete this function.
;; - [X] Prompt for and build proper filename
;; - [ ] Create named file
;; - [ ] Populate named file with pre-amble
(defun tor-post-new (title &optional)
  "Create a new draft blog post for the prompted TITLE."
  (interactive (list (read-from-minibuffer
		      "Title: "
		      nil nil nil nil)))

  (let ((default-directory (concat "~/git/takeonrules.github.io/content/posts/"
                                   (format-time-string "%Y/"))))
    (let ((filename (concat default-directory (s-dashed-words title) ".md")))
      (message filename)
    )))

(provide 'jnf-blogging.el)
;;; jnf--blogging.el ends here
