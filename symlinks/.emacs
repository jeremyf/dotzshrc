(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(require 'package)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(add-to-list 'package-archives
'("gnu" . "https://elpa.gnu.org/packages"))
(add-to-list 'package-archives
'("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(straight-use-package 'use-package)

;; https://oremacs.com/swiper/
(straight-use-package 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; https://oremacs.com/swiper/#completion-styles
;; Set all searches to regex fuzzy
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

;; Only the filename search is fuzzy
;;(setq ivy-re-builders-alist
;;      '((read-file-name-internal . ivy--regex-fuzzy)
;;        (t . ivy--regex-plus)))

;; https://docs.projectile.mx/en/latest/
;;(straight-use-package 'projectile)
;; (use-package projectile
;;   :ensure t
;;   :config
;;   (define-key projectile-mode-map (kbd "C-t") 'projectile-command-map)
;;   (projectile-mode +1))
;;   (setq projectile-project-search-path '("~/git/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages (quote (treemacs markdown-mode robe magit)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; With the base16-shell installed, I have found
;; that I need the "colors" setting.
(setq base16-theme-256-color-source "colors")
(setq base16-highlight-mode-line "contrast")
(straight-use-package 'base16-theme)
(use-package base16-theme
   :ensure t
   :config
   (load-theme 'base16-google-light t))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

  ;; (use-package treemacs
  ;;   :ensure t
  ;;   :defer t
  ;;   :init
  ;;   (with-eval-after-load 'winum
  ;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  ;;   :config
  ;;   (progn
  ;;     (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
  ;;           treemacs-deferred-git-apply-delay      0.5
  ;;           treemacs-directory-name-transformer    #'identity
  ;;           treemacs-display-in-side-window        t
  ;;           treemacs-eldoc-display                 t
  ;;           treemacs-file-event-delay              5000
  ;;           treemacs-file-extension-regex          treemacs-last-period-regex-value
  ;;           treemacs-file-follow-delay             0.2
  ;;           treemacs-file-name-transformer         #'identity
  ;;           treemacs-follow-after-init             t
  ;;           treemacs-git-command-pipe              ""
  ;;           treemacs-goto-tag-strategy             'refetch-index
  ;;           treemacs-indentation                   2
  ;;           treemacs-indentation-string            " "
  ;;           treemacs-is-never-other-window         nil
  ;;           treemacs-max-git-entries               5000
  ;;           treemacs-missing-project-action        'ask
  ;;           treemacs-move-forward-on-expand        nil
  ;;           treemacs-no-png-images                 nil
  ;;           treemacs-no-delete-other-windows       t
  ;;           treemacs-project-follow-cleanup        nil
  ;;           treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
  ;;           treemacs-position                      'left
  ;;           treemacs-recenter-distance             0.1
  ;;           treemacs-recenter-after-file-follow    nil
  ;;           treemacs-recenter-after-tag-follow     nil
  ;;           treemacs-recenter-after-project-jump   'always
  ;;           treemacs-recenter-after-project-expand 'on-distance
  ;;           treemacs-show-cursor                   nil
  ;;           treemacs-show-hidden-files             t
  ;;           treemacs-silent-filewatch              nil
  ;;           treemacs-silent-refresh                nil
  ;;           treemacs-sorting                       'alphabetic-asc
  ;;           treemacs-space-between-root-nodes      t
  ;;           treemacs-tag-follow-cleanup            t
  ;;           treemacs-tag-follow-delay              1.5
  ;;           treemacs-user-mode-line-format         nil
  ;;           treemacs-user-header-line-format       nil
  ;;           treemacs-width                         35)

  ;;     ;; The default width and height of the icons is 22 pixels. If you are
  ;;     ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;     ;;(treemacs-resize-icons 44)

  ;;     (treemacs-follow-mode t)
  ;;     (treemacs-filewatch-mode t)
  ;;     (treemacs-fringe-indicator-mode t)
  ;;     (pcase (cons (not (null (executable-find "git")))
  ;;                  (not (null treemacs-python-executable)))
  ;;       (`(t . t)
  ;;        (treemacs-git-mode 'deferred))
  ;;       (`(t . _)
  ;;        (treemacs-git-mode 'simple))))
  ;;   :bind
  ;;   (:map global-map
  ;;         ("M-0"       . treemacs-select-window)
  ;;         ("C-x t 1"   . treemacs-delete-other-windows)
  ;;         ("C-x t t"   . treemacs)
  ;;         ("C-x t B"   . treemacs-bookmark)
  ;;         ("C-x t C-t" . treemacs-find-file)
  ;;         ("C-x t M-t" . treemacs-find-tag)))

  ;; (use-package treemacs-projectile
  ;;   :after treemacs projectile
  ;;   :ensure t)

  ;; (use-package treemacs-icons-dired
  ;;   :after treemacs dired
  ;;   :ensure t
  ;;   :config (treemacs-icons-dired-mode))

  ;; (use-package treemacs-magit
  ;;   :after treemacs magit
  ;;   :ensure t)

  ;; (use-package treemacs-persp
  ;;   :after treemacs persp-mode
  ;;   :ensure t
  ;;   :config (treemacs-set-scope-type 'Perspectives))

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
