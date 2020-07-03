;;; package --- Summary:
;;
;;  Emacs configuration for Jeremy Friesen
;;
;;; Commentary:
;;
;;  This is my journey into Emacs.  Let's see where we go!
;;
;;; CODE:

(server-start)

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

;; Hide the icons of the Emacs toolbar
(tool-bar-mode -1)

;; The default is 60.  It is rare that I need more than 15 or 20.
;; However in my long use of Jumpcut there have been a few times where
;; I get into the 80s on previous pastes.  Given that the kill ring is
;; searchable via ivy/counsel, I think a larger value makes a lot of
;; sense.
(setq kill-ring-max 120)

;; Given the number of symlinks, visit the "linked to" file.
(setq vc-follow-symlinks t)

;; When you open a new frame in an already running Emacs session
;; set it to the full height but don't worry about the width
(setq-default indent-tabs-mode nil) ;; Ensure tabs are expanded, not inserted
(setq inhibit-startup-screen t) ;; Don't include the  emacs "start" window

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
;; See https://snarfed.org/gnu_emacs_backup_files
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; I saw that straight loaded use-package to take advantage of the
;; use-package syntax which is often how things are documented.
(straight-use-package 'use-package)

(use-package base16-theme
  :straight t
  :ensure t
  :config (load-theme 'base16-onedark t))
;; :config (load-theme 'base16-one-light t))

;; Remove reliance on Base16 themes; I find that I prefer the
;; coloration of tsdh-(light|dark)
;; (load-theme 'misterioso t) ;; For inside work
;; (load-theme 'adwaita t) ;; For bright days

;; Write "kill" command inputs to disk
(use-package savekill
  :ensure t
  :straight t
  :defer t)

;; https://oremacs.com/swiper/
;; Note: I've set all searches to use fuzzy regex
(use-package ivy
  :straight t
  :ensure t
  :after avy
  :diminish (ivy-mode . "")
  :bind (("C-c C-r". 'ivy-resume))
  :config (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 12)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-ignore-order))))

(use-package swiper
  :straight t
  :after ivy
  :ensure t
  :bind (("C-s" . swiper)))

;; Part of the ivy/counsel/swiper trio
(use-package counsel
  :straight t
  :after ivy
  :ensure t
  :init (setq ivy-use-selectable-prompt t)
  (setq search-default-mode #'char-fold-to-regexp)
  :bind (
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file) ;; A complimentary function to the
         ;; counsel-projectile-find-file;
         ;; this scopes to the current
         ;; director
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> o" . counsel-describe-symbol)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c r" . counsel-register)
         ("C-c m" . counsel-mark-ring))
  :config (counsel-mode 1))
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; The silver searcher; I found ripgrep a bit nicer, but wait until
;; you try wgrep-ag
(use-package ag
  :straight t
  :after counsel
  :defer t
  :ensure t)

;; This package is amazing!!!  Render search results to a buffer, edit
;; the buffer and write back to the file hits.  There is not a ripgrep
;; option.
(use-package wgrep-ag
  :ensure t
  :straight t
  :after ag
  :defer t)
(add-hook 'ag-mode-hook 'wgrep-ag-setup)

;; Search via ag, see candidates and use ivy to show ALL candidates,
;; then wgrep to edit those candidates and save
;;
;; 1) M-s-f 'counsel-ag
;; 2) C-c C-o 'ivy-occur
;; 3) C-c C-p 'wgrep-toggle-readonly-area
;; 4) C-x C-s to save OR C-x C-q to exit without save
(global-set-key (kbd "M-s-ƒ") 'counsel-ag) ;; CMD+OPT+f
(global-set-key (kbd "M-s-f") 'counsel-ag) ;; CMD+OPT+f

;; I have found this package quite "helpful"; When I want to know the
;; name of a function or key or variable, I can use the helpful
;; package.
(use-package helpful
  :straight t
  :ensure t
  :after counsel
  :defer t
  :bind (
         ("C-c C-d" . helpful-at-point)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h f" . helpful-function)
         ("C-h c" . helpful-command)
         ("C-h f" . helpful-callable)
         )
  :init (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  )

;; https://docs.projectile.mx/en/latest/
;;
;; Helpful for understanding the likely bounds of directory structure
(use-package projectile
  :ensure t
  :straight t
  :config (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/git/"))
  ;; Commented out for counsel-projectile
  ;; :init (global-set-key (kbd "s-t") 'projectile-find-file)
  :config (global-set-key (kbd "s-.") 'projectile-toggle-between-implementation-and-test)
  )

(use-package counsel-projectile
  :straight t
  :ensure t
  :defer t
  :after projectile
  :bind ("s-t" . counsel-projectile-find-file)) ; CMD+t

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
  :defer t
  :config (ac-config-default)
  :init (setq ac-auto-show-menu t))

(use-package inf-ruby
  :straight t
  :after auto-complete
  :defer t
  :ensure t)
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)

(use-package ac-inf-ruby
  :straight t
  :after inf-ruby
  :defer t
  :ensure t)
(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'inf-ruby-minor-mode))
(add-hook 'ruby-mode-hook 'ac-inf-ruby-enable)

(use-package string-inflection
  :defer t
  :ensure t
  :straight (string-inflection :type git :host github :repo "akicho8/string-inflection")
  :bind (("H-u" . string-inflection-all-cycle)
         ("C-M-u" . string-inflection-all-cycle)))

;; Allow to work with multipe cursors
;; https://melpa.org/#/multiple-cursors Aside from the
;; set-rectangular-region-anchor, there are several additional
;; features to practice
(use-package multiple-cursors
  :straight t
  :ensure   t
  :defer    1
  :bind (("H-SPC" . set-rectangular-region-anchor)
         ("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-s-l C-s-l" . mc/edit-lines) ;; CTRL+CMD+c
         ))

;; Adds some nice chrome to the status line
;; https://github.com/milkypostman/powerline Note: This is a faster
;; "line" package than spaceline. If I were not using emacs daemon, I
;; would use this.
;;
;; (use-package powerline
;;   :ensure t
;;   :straight t
;;   :defer 1
;;   :config (powerline-center-theme)
;;   )

(use-package spaceline
  :ensure t
  :straight t)

(use-package spaceline-all-the-icons
  :ensure t
  :straight t
  :after spaceline
  :config (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-neotree))

;; Eats all of the empty spaces when you type DELETE This is a bit
;; hungrier than I might want, so I'm keeping an eye on it.
(use-package hungry-delete
  :straight t
  :ensure t
  :defer t)
(global-hungry-delete-mode)

;; This package ensures that the active window gets the majority of
;; the space, while leaving room for other windows.
;;
;; Interestingly, having a visual reminder of the active window helps
;; focus my thinking.
(use-package golden-ratio
  :straight t
  :ensure t
  :config (golden-ratio-mode 1)
  )

;; A window manager for emacs, allowing fast toggles between windows
;; as well as opening or moving those windows.
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :straight t
  :ensure t
  :bind (("M-o" . ace-window)))

(use-package awesome-tab
  :ensure t
  :straight (awesome-tab :type git :host github :repo "manateelazycat/awesome-tab")
  :config (awesome-tab-mode t)
  (setq awesome-tab-height 130))
(global-set-key (kbd "s-1") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-2") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-3") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-4") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-5") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-6") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-7") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-8") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-9") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-0") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "C-c C-t") 'awesome-tab-counsel-switch-group)
(global-set-key [M-s-left] 'awesome-tab-backward-tab)
(global-set-key [M-s-right] 'awesome-tab-forward-tab)
(global-set-key [s-{] 'awesome-tab-backward-tab)
(global-set-key [s-}] 'awesome-tab-forward-tab)
(global-set-key (kbd "s-k") 'kill-current-buffer)
(global-set-key (kbd "s-w") 'ace-delete-window)

;; `C-u M-x scratch` prompts for a mode then creates a buffer in that
;; mode
(use-package scratch
  :ensure t
  :straight t
  :defer t)

;; `C-c s` will create a new buffer with the mode set to the current
;; buffer
(define-key (current-global-map) "\C-cs" #'scratch)


;; Adding smartparens options
(use-package smartparens
  :straight t
  :ensure t
  :defer t
  :config (smartparens-strict-mode 1)
  (smartparens-global-mode 1))

(use-package flycheck
  :ensure t
  :straight t
  :defer t
  :init
  (global-flycheck-mode t)
  ;; As much as I'd like to have reek and rubocop enabled, it turns
  ;; out they are quite chatty.  I also find that when syntax errors
  ;; are on I focus on fixing them and can lose sight of the
  ;; programming task I'm trying to accomplish.  In other words, while
  ;; writing, I don't want to see editing suggestions.
  (setq-default flycheck-disabled-checkers '(ruby-reek ruby-rubocop))
)

(use-package flyspell-correct
  :straight t
  :ensure t
  :after flycheck
  :defer t)

(use-package flyspell-correct-ivy
  :straight t
  :ensure t
  :defer t
  :after flyspell-correct)
(global-set-key (kbd "C-,") 'flyspell-buffer)

;; A rather convenient snippet manager.  When you create a snippet, it
;; understands the mode you're in and puts the snippet in the right
;; place.
(use-package yasnippets
  :straight (yasnippets :type git :host github :repo "joaotavora/yasnippet")
  :ensure t
  :defer t
  :bind (("C-c C-e" . yas-expand))
  :init (setq yas-snippet-dirs '("~/git/dotzshrc/emacs/snippets"))
  (yas-global-mode 1)
  )

(defalias 'tp 'transpose-pagraphs)
(defalias 'ts 'transpose-sentence)

;; Browse remote git repositories; A prior package I used limited the
;; behavior to Github. This one is bitbucket, github, sourcehut,
;; gitlab, etc.
(use-package browse-at-remote
  :straight t
  :ensure t
  :defer t
  :bind (("C-c g g" . browse-at-remote)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#fdf0ed" "#e95678" "#29d398" "#fadad1" "#26bbd9" "#ee64ac" "#26bbd9" "#403c3d"])
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 '(column-number-mode t)
 '(custom-safe-themes
   '("250268d5c0b4877cc2b7c439687f8145a2c85a48981f7070a72c7f47a2d2dc13" "23ba4b4ba4d1c989784475fed58919225db8d9a9751b32aa8df835134fe7ba6f" default))
 '(dired-listing-switches "-laGhpX")
 '(dired-use-ls-dired t)
 '(global-display-line-numbers-mode t)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Nice for neotree
(use-package all-the-icons
  :straight t
  :ensure t)

;; Favor neotree over sr-speedbar
(use-package neotree
  :straight t
  :ensure t
  :defer 1
  :init (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq-default neo-window-width 36)
  :config (global-set-key [f8] 'neotree-toggle))


;; Whitespace hygene package.  The author's documentation and
;; commentary echoes my sentiments
(use-package ethan-wspace
  :straight t
  :ensure t
  :defer t
  :init (setq-default mode-require-final-newline nil))
(global-ethan-wspace-mode 1)
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; (use-package column-enforce-mode
;;   :straight t
;;   :ensure t
;;   :defer t
;;   :init (setq column-enforce-column 80))
;; (global-column-enforce-mode t)

(use-package emmet-mode
  :straight t
  :ensure t)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.


;; https://github.com/jrblevin/markdown-mode/
(use-package markdown-mode
  :straight t
  :ensure t
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/pandoc"))

(use-package yaml-mode
  :straight t
  :ensure t
  :defer t)

;; Type C-; and a letter. That letter is the beginning of a
;; word. Narrow results from there.
(use-package avy
  :straight t
  :ensure t
  :defer 1
  :bind (("C-;" . avy-goto-word-1)))

;; I don't use a lot of folding, this allows me to type C-RET and fold
;; the current block.  There's more it can do but for now that's
;; enough
(use-package yafolding
  :straight t
  :ensure t
  :defer 1)
(add-hook 'prog-mode-hook
          (lambda () (yafolding-mode)))

(use-package json-mode
  :straight t
  :ensure t
  :defer 1)

;; Compressed JSON sure is ugly and illegible; This solves that
;; problem.
(use-package json-reformat
  :straight t
  :ensure t
  :after json-mode
  :defer 1
  :init (setq json-reformat:indent-width 2))

;; Adding format to git-commit-fill-column of 72 as best
;; practice.
(use-package magit
  :straight t
  :ensure t
  :defer 1 ;; This needs to be an integer. Key bindings fail when set to "t"
  :init (setq git-commit-fill-column 72)
  :bind (("H-g" . magit-status)
         ("C-M-g" . magit-status)))

;; With the time machine, travel back and forth through a files history
(use-package git-timemachine
  :straight t
  :ensure t
  :defer t)

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
  :bind (([C-s-down] . move-text-down)
         ([C-s-up] . move-text-up)))

;; And yes "nab" is not idiomatic, but since I'm mapping it to OPT+n,
;; I believe it will help me remember.
(defun nab-name-of-file ()
    "Copy into the kill ring the full path of the current buffer."
    (interactive)
    (kill-new (buffer-file-name (window-buffer (minibuffer-selected-window)))))


(global-set-key (kbd "M-n") 'nab-name-of-file) ;; OPT+n
(global-set-key (kbd "C-c C-b") 'browse-web) ;; CTRL+C CTRL+b
(global-set-key (kbd "C-s-w") 'browse-url-at-point) ;; CTRL+CMD+w
(global-set-key (kbd "s-b") 'switch-to-buffer) ;; CMD+b
(global-set-key (kbd "C-s-b") 'switch-to-buffer-other-window) ;; CTRL+CMD+b
(setq browse-url-browser-function 'eww-browse-url)

(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-DEL") 'backward-kill-paragraph)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)

;; https://melpa.org/#/elfeed
;; (global-set-key (kbd "C-x r") 'elfeed)

(if (eq system-type 'darwin)
    (progn (add-to-list 'load-path "~/git/dotzshrc/emacs/darwin")
           (require 'emacs-config.el)))

(if (eq system-type 'gnu/linux)
    (progn (add-to-list 'load-path "~/git/dotzshrc/emacs/gnu-linux")
           (require 'emacs-config.el)))

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
         "* %U %^{SUBJECT}\n\n    %?\n")
        ("l" "Today I Learned" entry (file+headline "~/git/org/2020/today-i-learned.org" "Today I Learned")
         "* %U: %?")
        ("m" "Meeting" entry (file+headline "~/git/org/2020/meetings.org" "Meetings")
         "* Meeting: %u %^{SUMMARY}\n\n  %^{ATTENDEES}p\n  %?\n")
        ("r" "Reading" entry (file+headline "~/git/org/2020/readings.org" "Readings")
         "* %^{SUBJECT} %u\n %?\n")
        ("s" "Session" entry (file+headline "~/git/org/2020/sessions.org" "Sessions")
         "* Session: %u %^{SUMMARY}\n\n  %^{ATTENDEES}p\n  %^{SYSTEM}p\n  %?\n")
        ("t" "Todo" entry (file+headline "~/git/org/2020/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("b" "Troubleshooting" entry (file+headline "~/git/org/2020/troubleshooting.org" "Trouble Shooting")
         "* %u Problem %^{SUMMARY}\n\n  %?\n  %a")
        ))

(use-package org-web-tools
  :defer t
  :ensure t
  :straight t)

;; END ORG mode configuration and concerns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A game
(use-package slime-volleyball
  :defer t
  :straight t
  :ensure t)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
