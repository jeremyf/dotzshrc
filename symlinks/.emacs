;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages (quote (markdown-mode robe magit base16-theme)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; https://github.com/ianpan870102/tron-legacy-emacs-theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'tron-legacy t)
;; (setq tron-legacy-dark-fg-bright-comments t)
;; (setq tron-legacy-vivid-cursor t)

;; https://melpa.org/#/robe
;; (global-robe-mode)

;; https://melpa.org/#/yard-mode
;; (add-hook 'ruby-mode-hook 'yard-mode)

;; https://melpa.org/#/rspec-mode
;; (add-hook 'dired-mode-hook 'rspec-dired-mode)

;; From https://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg)
)

;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist `(("." . "~/.saves")))

;; I want the duplicate line feature. In other editors, I had CMD+d.
;; CMD appears off limits in emacs. So we'll go with CTRL+SHIFT+d
(global-set-key (kbd "C-D") 'duplicate-line)
(global-set-key (kbd "C-x w") 'browse-url-at-point)

;; https://melpa.org/#/elfeed
;; (global-set-key (kbd "C-x r") 'elfeed)

;; TODO - Write macro to force wrap commit messages to 70 characters
;; (‹C-u f 20 M-q› for changing a paragraph to 20 character width)

;; TODO - Review pages I want to launch directly
;; https://github.com/dakrone/eos/blob/master/eos-web.org
