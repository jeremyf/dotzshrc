;;; package --- Summary:
;;
;;  Emacs configuration for Jeremy Friesen
;;
;;; Commentary:
;;
;;  This is my journey into Emacs.  Let's see where we go!
;;
;;; CODE:

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
;; From https://blog.d46.us/advanced-emacs-startup/
(setq gc-cons-threshold (* 250 1000 1000))

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

;; Run the following to print the load times:
;;
;; `emacs . --eval='(message "%s" (emacs-init-time))'`
;;
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs on \"%s\" ready in %s with %d garbage collections."
                     system-type
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; When you open Emacs, grab all the space on the screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Remove reliance on Base16 themes; I find that I prefer the
;; coloration of tsdh-(light|dark)
(load-theme 'misterioso t) ;; For inside work
;; (load-theme 'adwaita t) ;; For bright days

;; When you open a new frame in an already running Emacs session
;; set it to the full height but don't worry about the width
(add-to-list 'default-frame-alist '(fullscreen . fullheight))
(set-frame-font "MesloLGS NF 13" nil t)
(setq-default indent-tabs-mode nil) ;; Ensure tabs are expanded, not inserted
(setq inhibit-startup-screen t) ;; Don't include the  emacs "start" window

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
;; See https://snarfed.org/gnu_emacs_backup_files
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; I saw that straight loaded use-package to take advantage of the
;; use-package syntax which is often how things are documented.
(straight-use-package 'use-package)

;; https://oremacs.com/swiper/
;; Note: I've set all searches to use fuzzy regex
(use-package ivy
  :straight t
  :defer t
  :ensure t
  :diminish (ivy-mode . "")
  :bind (:map ivy-mode-map
              ("C-'" . ivy-avy))
  :config (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-ignore-order))))

(use-package counsel
  :straight t
  :defer t
  :ensure t
  :init (setq ivy-use-selectable-prompt t)
    (setq search-default-mode #'char-fold-to-regexp)
  :config
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
    (global-set-key (kbd "C-c r") 'counsel-rg)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (counsel-mode 1))


(use-package helpful
  :straight t
  :ensure t
  :defer 2
  :bind (("C-c C-d" . helpful-at-point))
  :init (setq counsel-describe-function-function #'helpful-callable)
        (setq counsel-describe-variable-function #'helpful-variable)
  :config (global-set-key (kbd "C-h f") #'helpful-callable)
        (global-set-key (kbd "C-h v") #'helpful-variable)
        (global-set-key (kbd "C-h k") #'helpful-key)
        (global-set-key (kbd "C-h f") #'helpful-function)
        (global-set-key (kbd "C-h c") #'helpful-command)
)

;; https://docs.projectile.mx/en/latest/
(use-package projectile
  :ensure t
  :straight t
  :config (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/git/"))
  ;; Commented out for counsel-projectile
  ;; :init (global-set-key (kbd "s-t") 'projectile-find-file)
  :config (global-set-key (kbd "s-.") 'projectile-toggle-between-implementation-and-test))

;; I'm liking ripgrep as a tool
(use-package projectile-ripgrep
  :ensure t
  :straight t
  :defer 1
  :init (global-set-key (kbd "M-s-ƒ") 'projectile-ripgrep) ;; CMD+OPT+f
  (global-set-key (kbd "M-s-f") 'projectile-ripgrep) ;; CMD+OPT+f
 )

(use-package counsel-projectile
  :straight t
  :ensure t
  :init (global-set-key (kbd "s-t") 'counsel-projectile-find-file))

(use-package robe
  :straight t
  :ensure t
  :defer t)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

(use-package rspec-mode
  :straight t
  :ensure t
  :defer t)
(add-hook 'ruby-mode-hook 'rspec-mode)

(use-package yard-mode
  :straight t
  :ensure t
  :defer t)
(add-hook 'ruby-mode-hook 'yard-mode)

(setq ruby-insert-encoding-magic-comment nil)

(use-package auto-complete
  :straight t
  :ensure t
  :config (ac-config-default)
  :init (setq ac-auto-show-menu t))

(use-package inf-ruby
  :straight t
  :after auto-complete
  :ensure t)
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)

(use-package ac-inf-ruby
  :straight t
  :after inf-ruby
  :ensure t)
(eval-after-load 'auto-complete
      '(add-to-list 'ac-modes 'inf-ruby-minor-mode))
    (add-hook 'ruby-mode-hook 'ac-inf-ruby-enable)

(use-package string-inflection
  :defer t
  :ensure t
  :straight (string-inflection :type git :host github :repo "akicho8/string-inflection"))
(global-set-key (kbd "H-u") 'string-inflection-all-cycle)
(global-set-key (kbd "C-M-u") 'string-inflection-all-cycle)

;; Allow to work with multipe cursors
;; https://melpa.org/#/multiple-cursors Aside from the
;; set-rectangular-region-anchor, there are several additional
;; features to practice
(use-package multiple-cursors
  :straight t
  :ensure   t
  :defer    1
  :config (global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)
  (global-set-key (kbd "C-M-SPC") 'set-rectangular-region-anchor)
)

;; Adds some nice chrome to the status line
;; https://github.com/milkypostman/powerline
(use-package powerline
  :ensure t
  :straight t
  :defer 2
  :config (powerline-center-theme)
)

;; This package ensures that the active window gets the majority of
;; the space, while leaving room for other windows.
(use-package golden-ratio
  :straight t
  :ensure t
  :config (golden-ratio-mode 1)
)

;; Adding smartparens options
(use-package smartparens
  :straight t
  :ensure t
  :config (smartparens-strict-mode 1)
        (smartparens-global-mode 1))

(use-package flycheck
  :ensure t
  :straight t
  :defer t
  :init
  (global-flycheck-mode t))

(use-package flyspell-correct
  :straight t
  :ensure t
  :after flyspell
  :defer t)

(use-package flyspell-correct-ivy
  :straight t
  :ensure t
  :after flyspell-correct)
(global-set-key (kbd "C-,") 'flyspell-buffer)


(use-package yasnippets
  :straight (yasnippets :type git :host github :repo "joaotavora/yasnippet")
  :ensure t
  :defer t
  :init (setq yas-snippet-dirs '("~/git/dotzshrc/emacs/snippets"))
  (yas-global-mode 1)
  )

(global-set-key (kbd "C-t") 'yas-expand)

(use-package github-browse-file
  :straight t
  :ensure t
  :defer t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#fdf0ed" "#e95678" "#29d398" "#fadad1" "#26bbd9" "#ee64ac" "#26bbd9" "#403c3d"])
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("250268d5c0b4877cc2b7c439687f8145a2c85a48981f7070a72c7f47a2d2dc13" "23ba4b4ba4d1c989784475fed58919225db8d9a9751b32aa8df835134fe7ba6f" default)))
 '(dired-listing-switches "-laGhpX")
 '(dired-use-ls-dired t)
 '(global-display-line-numbers-mode t)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package all-the-icons
  :straight t
  :ensure t)

;; Favor neotree over sr-speedbar
(use-package neotree
  :straight t
  :ensure t
  :defer 1
  :init (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :config (global-set-key [f8] 'neotree-toggle))

;; Whitespace   hygene  package.   The   author's  documentation   and
;; commentary echoes my sentiments
(use-package ethan-wspace
  :straight t
  :ensure t
  :defer 2
  :init (setq-default mode-require-final-newline nil)
  :config (global-ethan-wspace-mode 1)
) ;; Prefer the following ethan-wspac-mode

;; https://github.com/jrblevin/markdown-mode/
(use-package markdown-mode
  :straight t
  :ensure t
  :defer 3
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/pandoc"))

;; Type C-; and a letter. That letter is the beginning of a
;; word. Narrow results from there.
(use-package avy
  :straight t
  :ensure t
  :defer 1)
(global-set-key (kbd "C-;") 'avy-goto-word-1)

(use-package yafolding
  :straight t
  :ensure t
  :defer 1)

(add-hook 'prog-mode-hook
          (lambda () (yafolding-mode)))

(use-package json-reformat
  :straight t
  :ensure t
  :defer 1
  :init (setq json-reformat:indent-width 2))

(use-package json-mode
  :straight t
  :ensure t
  :defer 1)

;; Adding format to git-commit-fill-column of 72 as best
;; practice.
(use-package magit
  :straight t
  :ensure t
  :defer 2
  :init (setq git-commit-fill-column 72)
  :config (global-set-key (kbd "H-g") 'magit-status)
          (global-set-key (kbd "C-M-g") 'magit-status)
)

;; Copied from https://github.com/magit/magit/blob/9423edc0b311117ab5fe87457c3e01c7db22a3c7/lisp/git-commit.el
;; And set to 50 instead of 68
(defcustom git-commit-summary-max-length 50
  "Column beyond which characters in the summary lines are highlighted.
The highlighting indicates that the summary is getting too long
by some standards.  It does in no way imply that going over the
limit a few characters or in some cases even many characters is
anything that deserves shaming.  It's just a friendly reminder
that if you can make the summary shorter, then you might want
to consider doing so."
  :group 'git-commit
  :safe 'numberp
  :type 'number)

;; Adding ability to move lines up and down
(use-package move-text
  :straight t
  :ensure t
  :defer 1
  :init (global-set-key [C-s-down] 'move-text-down)
        (global-set-key [C-s-up] 'move-text-up))

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


;; And yes "nab" is not idiomatic, but since I'm mapping it to OPT+n, I believe it will help me remembe
(defun nab-name-of-file ()
    "Copy into the kill ring the full path of the current buffer."
    (interactive)
    (kill-new (buffer-file-name (window-buffer (minibuffer-selected-window)))))


(global-set-key (kbd "M-n") 'nab-name-of-file) ;; OPT+n
(global-set-key (kbd "C-M-d") 'duplicate-line) ;; CTRL+OPT+d - duplicate line
(global-set-key (kbd "C-s-w") 'browse-url-at-point) ;; CTRL+CMD+w
(global-set-key (kbd "s-w") 'browse-web) ;; CMD+w
(global-set-key (kbd "s-b") 'switch-to-buffer) ;; CMD+b
(global-set-key (kbd "C-s-b") 'switch-to-buffer-other-window) ;; CTRL+CMD+b
(setq browse-url-browser-function 'eww-browse-url)

;; Because smie-down-list grabbed C-M-d, I need to set it
(defun jnf-add-duplicate-line-kbd()
  (local-set-key (kbd "C-M-d") 'duplicate-line))
(add-hook 'ruby-mode-hook 'jnf-add-duplicate-line-kbd)

(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-DEL") 'backward-kill-paragraph)

;; https://melpa.org/#/elfeed
;; (global-set-key (kbd "C-x r") 'elfeed)

;; Adds the ability to grab a link from various OS X applications
;; Note, the sibling org-mac-link. That package works within ORG mode
;; with an extended menu option, and assumes ORG styling
(use-package grab-mac-link
  :ensure t
  :straight t
  :defer 1)
(global-set-key (kbd "C-c C-g") 'grab-mac-link)

(use-package org-mac-link
  :ensure t
  :straight (org-mac-link :type git :host github :repo "jeremyf/org-mac-link")
  :defer t)
(add-hook 'org-mode-hook (lambda ()
                           (define-key org-mode-map (kbd "C-c C-g") 'org-mac-grab-link)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN ORG mode configuration and concerns
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-directory "~/git/org")
(setq org-default-notes-file (concat org-directory "/captured-notes.org"))

;; I'm working through what templates I might want. This is a place holder.
(setq org-capture-templates
      '(
        ("g" "Game Idea" entry (file+headline "~/git/org/2020/game-ideas.org" "Game Idea")
         "* %?\nEntered on %U\n  %i")
        ("j" "Journal" entry (file+datetree "~/git/org/2020/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("l" "Today I Learned" entry (file+headline "~/git/org/2020/today-i-learned.org" "Today I Learned")
         "* %U: %?")
        ("m" "Meeting" entry (file+headline "~/git/org/2020/meetings.org" "Meetings")
         "* Meeting: %u %^{SUMMARY}\n  %^{ATTENDEES}p\n  %?\n")
        ("r" "Reading" entry (file+headline "~/git/org/2020/sessions.org" "Readings")
         "* %^{SUBJECT} %u\n %?\n")
        ("s" "Session" entry (file+headline "~/git/org/2020/sessions.org" "Sessions")
         "* Session: %u %^{SUMMARY}\n  %^{ATTENDEES}p\n  %^{SYSTEM}p\n  %?\n")
        ("t" "Todo" entry (file+headline "~/git/org/2020/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ))

;; END ORG mode configuration and concerns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
