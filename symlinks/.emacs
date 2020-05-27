;; This preamble is part of straight-use-package My understanding, in
;; reading straight documentation is that it has better load
;; times. However, the configuration options I often see leverage
;; "use-package" which is why most of my package declarations look as
;; they do.
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

;; I saw that straight loaded use-package to take advantage of the
;; use-package syntax which is often how things are documented.
(straight-use-package 'use-package)

;; https://oremacs.com/swiper/
;; Note: I've set all searches to use fuzzy regex
(use-package ivy
  :straight t
  :defer 1
  :ensure t
  :init (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-count-format        "(%d/%d) "
        ivy-re-builders-alist   '((read-file-name-internal . ivy--regex-fuzzy)
				  (t . ivy--regex-ignore-order))))
(ivy-mode 1)

(use-package counsel
  :straight t
  :defer 1
  :ensure t)
(counsel-mode 1)

(setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; https://docs.projectile.mx/en/latest/
;;(straight-use-package 'projectile)
;; (use-package projectile
;;   :ensure t
;;   :config
;;   (define-key projectile-mode-map (kbd "C-t") 'projectile-command-map)
;;   (projectile-mode +1))
;;   (setq projectile-project-search-path '("~/git/"))

(straight-use-package '(string-inflection :type git :host github :repo "akicho8/string-inflection"))
(global-set-key (kbd "H-u") 'string-inflection-all-cycle)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-display-line-numbers-mode t)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; https://github.com/jrblevin/markdown-mode/
(use-package markdown-mode
  :straight t
  :ensure t
  :defer 1
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/pandoc"))

(use-package robe
  :straight t
  :ensure t
  :defer 1)

;; With the base16-shell installed, I have found
;; that I need the "colors" setting.
(use-package base16-theme
  :straight t
  :ensure t
  :config (load-theme 'base16-google-light t)
  :init (setq base16-theme-256-color-source "colors"
	        base16-highlight-mode-line "contrast"))

(use-package magit
  :straight t
  :ensure t
  :defer 1)
(global-set-key (kbd "H-g") 'magit-status)

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

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
;; See https://snarfed.org/gnu_emacs_backup_files
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; I want the duplicate line feature. In other editors, I had CMD+SHIFT+d.
;; Emacs exposes Super (s-), Hyper (H-). And while my keyboard doesn't have
;; those keys, I can use ITerm to mimic the behavior.
;; See https://www.emacswiki.org/emacs/iTerm2#toc11
(global-set-key (kbd "H-d") 'duplicate-line)
(global-set-key (kbd "C-x w") 'browse-url-at-point)

;; According to emacs help, (comment-line) is bound to "C-x C-;".
;; However, when I invoked those keys, it ran comment-column. As
;; that is undesired behavior, I mapped (comment_line) to "H-c"
(global-set-key (kbd "H-/") 'comment-line)
(global-set-key (kbd "H-l") 'goto-line)

;; Invoke `s-a` with CTRL+OPT+CMD+a
;; Invoke `H-a` with CTRL+OPT+a
(progn
     (let ((map (if (boundp 'input-decode-map)
                 input-decode-map
     function-key-map)))
     (define-key map "\e[1;P9"  (kbd "s-a"))
     (define-key map "\e[1;P10" (kbd "s-b"))
     (define-key map "\e[1;P11" (kbd "s-c"))
     (define-key map "\e[1;P12" (kbd "s-d"))
     (define-key map "\e[1;P13" (kbd "s-e"))
     (define-key map "\e[1;P14" (kbd "s-f"))
     (define-key map "\e[1;P15" (kbd "s-g"))
     (define-key map "\e[1;P16" (kbd "s-h"))
     (define-key map "\e[1;P17" (kbd "s-i"))
     (define-key map "\e[1;P18" (kbd "s-j"))
     (define-key map "\e[1;P19" (kbd "s-k"))
     (define-key map "\e[1;P20" (kbd "s-l"))
     (define-key map "\e[1;P21" (kbd "s-m"))
     (define-key map "\e[1;P22" (kbd "s-n"))
     (define-key map "\e[1;P23" (kbd "s-o"))
     (define-key map "\e[1;P24" (kbd "s-p"))
     (define-key map "\e[1;P25" (kbd "s-q"))
     (define-key map "\e[1;P26" (kbd "s-r"))
     (define-key map "\e[1;P27" (kbd "s-s"))
     (define-key map "\e[1;P28" (kbd "s-t"))
     (define-key map "\e[1;P29" (kbd "s-u"))
     (define-key map "\e[1;P30" (kbd "s-v"))
     (define-key map "\e[1;P31" (kbd "s-w"))
     (define-key map "\e[1;P32" (kbd "s-x"))
     (define-key map "\e[1;P33" (kbd "s-y"))
     (define-key map "\e[1;P34" (kbd "s-z"))
     (define-key map "\e[1;P35" (kbd "H-a"))
     (define-key map "\e[1;P36" (kbd "H-b"))
     (define-key map "\e[1;P37" (kbd "H-c"))
     (define-key map "\e[1;P38" (kbd "H-d"))
     (define-key map "\e[1;P39" (kbd "H-e"))
     (define-key map "\e[1;P40" (kbd "H-f"))
     (define-key map "\e[1;P41" (kbd "H-g"))
     (define-key map "\e[1;P42" (kbd "H-h"))
     (define-key map "\e[1;P43" (kbd "H-i"))
     (define-key map "\e[1;P44" (kbd "H-j"))
     (define-key map "\e[1;P45" (kbd "H-k"))
     (define-key map "\e[1;P46" (kbd "H-l"))
     (define-key map "\e[1;P47" (kbd "H-m"))
     (define-key map "\e[1;P48" (kbd "H-n"))
     (define-key map "\e[1;P49" (kbd "H-o"))
     (define-key map "\e[1;P50" (kbd "H-p"))
     (define-key map "\e[1;P51" (kbd "H-q"))
     (define-key map "\e[1;P52" (kbd "H-r"))
     (define-key map "\e[1;P53" (kbd "H-s"))
     (define-key map "\e[1;P54" (kbd "H-t"))
     (define-key map "\e[1;P55" (kbd "H-u"))
     (define-key map "\e[1;P56" (kbd "H-v"))
     (define-key map "\e[1;P57" (kbd "H-w"))
     (define-key map "\e[1;P58" (kbd "H-x"))
     (define-key map "\e[1;P59" (kbd "H-y"))
     (define-key map "\e[1;P60" (kbd "H-z"))
     (define-key map "\e[1;P70" (kbd "H-/"))
     (define-key map "\e[1;P71" (kbd "H-."))
     (define-key map "\e[1;P72" (kbd "H-,"))
     ))


;; https://melpa.org/#/elfeed
;; (global-set-key (kbd "C-x r") 'elfeed)

;; TODO - Review pages I want to launch directly
;; https://github.com/dakrone/eos/blob/master/eos-web.org
