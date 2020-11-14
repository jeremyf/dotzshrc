;;; ox-tor-md.el --- Summary
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;  This package provides the basis for exporting an org file to a
;;  Take on Rules compliant markdown file.
;;
;;  - [X] Add a menu-option to export Take on Rules buffer.
;;  - [ ] Modify the translate-alist to handle markdown accordingly
;;  - [ ] Determine how to invoke the export function without menu (I
;;        believe it would be org-export-to-buffer, but would want to
;;        pass - the named buffer in the TOR project)
;;
;;  First, I want to export an org-file to a basic markdown format
;;  that:
;;
;;  - Works with Hugo's markdown rendering
;;  - Uses org-babel and includes to build the exported content.
;;  - Adds a valid YAML front-matter
;;  - Works with my existing Take on Rules build chain.
;;
;;  Once I have the export working for a single file, I want to always
;;  write a given file to the same "path" in Take on Rules.  That way
;;  I can treat my org-mode file as the canonical source of truth.
;;
;;  An assumption when exporting is that:
;;
;;  1.  I will skip comments or comment nodes; Those are referee notes.
;;  2.  I will treat any org links to files as links to Take on Rules
;;      resources.  This may require a map table (e.g. path for org to
;;      path for takeonrules.com).
;;
;;  With the exports working, I want the ability to "push" those
;;  exports out.
;;
;;; Code:
;; ;; Consider https://github.com/jkitchin/org-ref as well
(require 's)
(org-export-define-derived-backend 'take-on-rules 'md
  :translate-alist '(
                     (headline . org-tor-headline)
                     (timestamp . org-tor-timestamp)
                     )
  :menu-entry
  '(?T "Export to Take on Rules"
       ((?f "As Markdown buffer" org-tor-export-as-markdown))))


(defun org-tor-headline (headline contents info)
  (let* ((low-level-rank (org-export-low-level-p headline info))
         (text (org-export-data (org-element-property :title headline)
                                info))
         (level (org-export-get-relative-level headline info))
         (prefix (s-repeat level "#")))
    (format "%s %s\n\n%s"
            prefix
            text
            (if (org-string-nw-p contents) contents ""))))

(defun org-tor-timestamp (timestamp _contents _info)
  "Transcode a TIMESTAMP object from Org to ToR.
CONTENTS and INFO are ignored."
  (let ((time (org-timestamp-to-time timestamp)))
    (format
     "<time datetime=\"%s\">%s</time>"
     (format-time-string "%Y-%m-%d" time)
     (format-time-string "%B %d, %Y" time)
     )))

(defun org-tor-export-as-markdown
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title, table
of contents and footnote definitions from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*org Take on Rules Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'take-on-rules "*org Take on Rules Export*"
    async subtreep visible-only t ext-plist (lambda () (text-mode))))
(provide 'ox-tor-md.el)
;;; ox-tor-md.el ends here
