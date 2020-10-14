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
(set-frame-font "MesloLGS NF")

;; A chunk of code that allows me to pass multiple filenames to
;; emacsclient AND open those files in different frames within the
;; same window.(defvar server-visit-files-custom-find:buffer-count)
(defvar server-visit-files-custom-find:buffer-count)
(defadvice server-visit-files
    (around server-visit-files-custom-find
            activate compile)
  "Maintain a counter of visited files from a single client call."
  (let ((server-visit-files-custom-find:buffer-count 0))
    ad-do-it))
(defun server-visit-hook-custom-find ()
  "Arrange to visit the files from a client call in separate windows."
  (if (zerop server-visit-files-custom-find:buffer-count)
      (progn
        (delete-other-windows)
        (switch-to-buffer (current-buffer)))
    (let ((buffer (current-buffer))
          (window (split-window-sensibly)))
      (switch-to-buffer buffer)
      (balance-windows)))
  (setq server-visit-files-custom-find:buffer-count
        (1+ server-visit-files-custom-find:buffer-count)))
(add-hook 'server-visit-hook 'server-visit-hook-custom-find)

;; I have additional files that I require in the emacs directory
(add-to-list 'load-path "~/git/dotzshrc/emacs")

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
;; From https://blog.d46.us/advanced-emacs-startup/
(setq gc-cons-threshold (* 250 1000 1000))

;; Stop ringing any bell
(setq ring-bell-function 'ignore)

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

;; Hide the scroll bar. Let's be clear, I don't use it.
(scroll-bar-mode -1)

;; Instead of typing "yes" or "no" short-circuit to "y" or "n"
(defalias 'yes-or-no-p 'y-or-n-p)

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

;; I'm just going to trust themes
(setq custom-safe-themes t)

(use-package modus-operandi-theme
  :ensure t
  :straight t
  :init
  (setq modus-operandi-theme-org-blocks 'rainbow)
  (setq modus-operandi-theme-completions 'opinionated)
  (setq modus-operandi-theme-fringes 'subtle)
  (setq modus-operandi-theme-scale-headings t
        modus-operandi-theme-slanted-constructs t
        modus-operandi-theme-bold-constructs t
        modus-operandi-theme-faint-syntax nil
        modus-operandi-theme-intense-hl-line nil
        modus-operandi-theme-mode-line 'moody
        modus-operandi-theme-completions 'opinionated
        modus-operandi-theme-intense-paren-match t)
  ;; :custom-face
  ;; I'd like to use the following, but I get interpretter errors:
  ;;
  :config
  ;; To determine the face at point run kbd "C-u C-x =", the "C-x ="
  ;; is 'what-cursor-position, the prefix of C-u indicates to render
  ;; the detailed version of 'what-cursor-position
  (custom-set-faces
   `(font-lock-variable-name-face ((t(:foreground ,(cdr(assoc "blue" modus-operandi-theme-default-colors-alist))
                                     :background "#e6edff"))))
   `(font-lock-string-face ((t(:foreground ,(cdr(assoc "green" modus-operandi-theme-default-colors-alist))
                                           :weight bold
                                           :background "#f2f7ed"))))
   )
  (defadvice load-theme (before theme-dont-propagate activate)
    "Disable theme before loading new one."
    (mapc #'disable-theme custom-enabled-themes))
  (load-theme 'modus-operandi t))

(use-package modus-vivendi-theme
  :straight t
  :ensure t)

(defun modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn
        (disable-theme 'modus-operandi)
        (load-theme 'modus-vivendi t))
    (disable-theme 'modus-vivendi)
    (load-theme 'modus-operandi t)))

;; "I find it useful to have a slightly more apparent indicator of which buffer is active at the moment."
;; https://blog.sumtypeofway.com/posts/emacs-config.html
;;
;; I want this to work but I keep experiencing a flicker; I believe
;; there's a background buffer that gets momentary focus.
;;
;; (use-package dimmer
;;   :ensure t
;;   :straight t
;;   :custom (dimmer-fraction 0.4)
;;   :config (dimmer-mode))

;; Doing a bit of configuration of my cursors
(setq-default cursor-type 'bar)
(set-cursor-color "#44B4CC") ;; The text color of my
(blink-cursor-mode t)

;; Remove reliance on Base16 themes; I find that I prefer the
;; coloration of tsdh-(light|dark)
;; (load-theme 'misterioso t) ;; For inside work
;; (load-theme 'adwaita t) ;; For bright days

;; Write "kill" command inputs to disk
(use-package savekill
  :ensure t
  :straight t)

;; https://oremacs.com/swiper/
;; Note: I've set all searches to use fuzzy regex
(use-package ivy
  :straight t
  :ensure t
  :after avy
  :diminish (ivy-mode . "")
  :bind (
         ("C-c C-r". 'ivy-resume)
         ("<f6>". 'ivy-resume)
         )
  :config (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 12)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-ignore-order))))


;; Part of the ivy/counsel/swiper trio
(use-package counsel
  :straight t
  :ensure t
  :init (setq ivy-use-selectable-prompt t)
  (setq search-default-mode #'char-fold-to-regexp)
  :bind (("M-x" . counsel-M-x))
  :config (counsel-mode 1))
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(defalias 'recent 'counsel-recentf)

(use-package all-the-icons-ivy-rich
  :ensure t
  :straight t
  :after (ivy counsel counsel-projectile)
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :ensure t
  :straight t
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer nil)
  (ivy-rich-path-style 'full)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode))


(use-package swiper
  :straight t
  :ensure t
  :bind (("C-s" . swiper)))

(use-package prescient
  :straight t
  :ensure t)

(use-package ivy-prescient
  :straight t
  :ensure t
  :init (setq prescient-filter-method '(literal fuzzy regexp initialism))
  :config (ivy-prescient-mode t))

(use-package expand-region
  :straight t
  :ensure t
  :bind (
         ("C-=" . er/expand-region)
         ("C-+" . er/contract-region)
         ))

;; The silver searcher; I found ripgrep a bit nicer, but wait until
;; you try wgrep-ag
(use-package ag
  :straight t
  :after counsel
  :bind (("M-s-f" . counsel-ag))
  :ensure t)

;; This package is amazing!!!  Render search results to a buffer, edit
;; the buffer and write back to the file hits.  There is not a ripgrep
;; option.
;;
;; Search via ag, see candidates and use ivy to show ALL candidates,
;; then wgrep to edit those candidates and save
;;
;; 1) M-s-f 'counsel-ag
;; 2) C-c C-o 'ivy-occur
;; 3) C-c C-p 'wgrep-toggle-readonly-area
;; 4) C-x C-s to save OR C-x C-q to exit without save
(use-package wgrep-ag
  :ensure t
  :straight t
  :hook (ag-mode . wgrep-ag-setup)
  :after ag)


;; I have found this package quite "helpful"; When I want to know the
;; name of a function or key or variable, I can use the helpful
;; package.
(use-package helpful
  :straight t
  :ensure t
  :bind (
         ("C-h C-d" . helpful-at-point)
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
  :after projectile
  :bind ("s-t" . counsel-projectile-find-file)) ; CMD+t, which I carry over from Textmate


(use-package company
  :straight t
  :ensure t
  :after ivy
  :diminish
  :bind (("C-." . #'company-complete))
  :hook (prog-mode . company-mode)
  :custom
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 20 "The more the merrier.")
  (company-tooltip-idle-delay 0.4 "Faster!")
  (company-async-timeout 20 "Some requests can take a long time. That's fine.")
  :config

  ;; Use the numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9))))

(use-package string-inflection
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
  :bind (("H-SPC" . set-rectangular-region-anchor)
         ("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines) ;; CTRL+CMD+c
         ))

;; C-; to select words
(use-package iedit
  :straight t
  :ensure t)

(global-so-long-mode)

;; Parenthesis matching is one of the flaws in my Emacs setup as of
;; this writing. I know that there are a lot of options out
;; there—paredit, smartparens, etc.—but I haven’t sat down and really
;; capital-L Learned a better solution than the TextMate-style bracket
;; completion (which Emacs calls, somewhat fancifully, ‘electric’).
;;
;; https://blog.sumtypeofway.com/posts/emacs-config.html
(electric-pair-mode)


(use-package spaceline
  :ensure t
  :straight t)

(use-package spaceline-all-the-icons
  :ensure t
  :straight t
  :after spaceline
  :config (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-neotree))

;; This package ensures that the active window gets the majority of
;; the space, while leaving room for other windows.
;;
;; Interestingly, having a visual reminder of the active window helps
;; focus my thinking.
;; (use-package golden-ratio
;;   :straight t
;;   :ensure t
;;   :config (golden-ratio-mode nil))
;; (setq golden-ratio-adjust-factor nil)

;; A window manager for emacs, allowing fast toggles between windows
;; as well as opening or moving those windows.
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :straight t
  :ensure t
  :bind (("M-o" . ace-window)))


;; "The long-awaited Emacs 27 support for native tabs is shaky, both
;; visually and in terms of functionality. As such, centaur-tabs is
;; the best way to simulate a conventional tabs setup, in which tab
;; sets are grouped by the toplevel project working directory."
;; https://blog.sumtypeofway.com/posts/emacs-config.html
(use-package centaur-tabs
  :straight t
  :ensure t
  :demand
  :hook
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-show-navigation-buttons t
        centaur-tabs-set-icons t
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-set-bar 'under)
  (centaur-tabs-headline-match)
  :custom
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-style "rounded")
  (centaur-tabs-height 36)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "●")
  (centaur-tabs-buffer-groups-function #'centaur-tabs-projectile-buffer-groups)
  :bind
  (
   ("s-{" . #'centaur-tabs-backward)
   ("s-}" . #'centaur-tabs-forward)
   ("C-c C-5". #'centaur-tabs-extract-window-to-new-frame)
   ([s-up] . #'centaur-tabs-backward-group)
   ([s-down] . #'centaur-tabs-forward-group)
   ("C-s-t" . #'centaur-tabs-counsel-switch-group)
   ("C-c C-o" . #'centaur-tabs-open-directory-in-external-application)
   )
  )

(use-package spatial-navigate
  :straight (spatial-navigate :type git :host gitlab :repo "ideasman42/emacs-spatial-navigate")
  :ensure t
  :bind
  (
   ("<M-s-up>" . #'spatial-navigate-backward-vertical-bar)
   ("<M-s-down>" . #'spatial-navigate-forward-vertical-bar)
   ("<M-s-left>" . #'spatial-navigate-backward-horizontal-bar)
   ("<M-s-right>" . #'spatial-navigate-forward-horizontal-bar)
   )
  )


;; Adding smartparens options
(use-package smartparens
  :straight t
  :ensure t
  :config (smartparens-strict-mode 1)
  (smartparens-global-mode 1))



;; A rather convenient snippet manager.  When you create a snippet, it
;; understands the mode you're in and puts the snippet in the right
;; place.
(use-package yasnippets
  :straight (yasnippets :type git :host github :repo "joaotavora/yasnippet")
  :ensure t

  :after company
  :config (push 'company-yasnippet company-backends)
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

  :bind (("C-c b" . browse-at-remote)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#fdf0ed" "#e95678" "#29d398" "#fadad1" "#26bbd9" "#ee64ac" "#26bbd9" "#403c3d"])
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(column-number-mode t)
 '(custom-safe-themes
   '("7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "8607fdf417935af22922d10b4664a4ead5a64c01b55ac9e4eb9f4da9d177f612" "250268d5c0b4877cc2b7c439687f8145a2c85a48981f7070a72c7f47a2d2dc13" "23ba4b4ba4d1c989784475fed58919225db8d9a9751b32aa8df835134fe7ba6f" default))
 '(delete-selection-mode t)
 '(dired-listing-switches "-laGhpX")
 '(dired-use-ls-dired t)
 '(flycheck-checkers
   '(ada-gnat asciidoctor asciidoc awk-gawk bazel-buildifier c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint cuda-nvcc cwl d-dmd dockerfile-hadolint elixir-credo emacs-lisp emacs-lisp-checkdoc ember-template erlang-rebar3 erlang eruby-erubis eruby-ruumba fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-staticcheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-standard json-jsonlint json-python-json json-jq jsonnet less less-stylelint llvm-llc lua-luacheck lua markdown-markdownlint-cli markdown-mdl nix nix-linter opam perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc protobuf-prototool pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile python-mypy r-lintr racket rpm-rpmlint rst-sphinx rst ruby-standard ruby-rubylint ruby ruby-jruby rust-cargo rust rust-clippy scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar terraform terraform-tflint tex-chktex tex-lacheck texinfo textlint typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby yaml-yamllint))
 '(global-display-line-numbers-mode t)
 '(org-agenda-files
   '("/Users/jfriesen/git/org/agenda.org" "/Users/jfriesen/git/org/elfeed.org" "/Users/jfriesen/git/org/index.org" "/Users/jfriesen/git/org/readings.org" "/Users/jfriesen/git/org/sessions.org" "/Users/jfriesen/git/org/troubleshooting.org"))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; C-a goes to the first non-whitepsace character on the line. Type it
;; again, and go to the beginning of the line.
(use-package crux
  :straight t
  :ensure t

  :bind (
         ("C-a" . crux-move-beginning-of-line)
         ("<f9>" . crux-kill-other-buffers)
         ))

;; Nice for neotree
(use-package all-the-icons
  :straight t
  :ensure t)

(use-package all-the-icons-dired
  :straight t
  :ensure t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; Favor neotree over sr-speedbar
(use-package neotree
  :straight t
  :ensure t

  :init (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq-default neo-window-width 36)
  :config (global-set-key [f8] 'neotree-toggle))


;; Whitespace hygene package.  The author's documentation and
;; commentary echoes my sentiments
(use-package ethan-wspace
  :straight t
  :ensure t

  :init (setq-default mode-require-final-newline nil))
(global-ethan-wspace-mode 1)
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; (use-package column-enforce-mode
;;   :straight t
;;   :ensure t
;;
;;   :init (setq column-enforce-column 80))
;; (global-column-enforce-mode t)

(use-package unfill
  :straight t
  :ensure t
  )

(use-package emmet-mode
  :straight t
  :ensure t)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.


;; https://github.com/jrblevin/markdown-mode/
(use-package markdown-mode
  :straight t
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/pandoc"))

(use-package yaml-mode
  :straight t
  :ensure t
  )

;; That letter is the beginning of a word. Narrow results from there.
(use-package avy
  :straight t
  :ensure t
  )

;; I don't use a lot of folding, this allows me to type C-RET and fold
;; the current block.  There's more it can do but for now that's
;; enough
(use-package yafolding
  :straight t
  :ensure t
  :hook (prog-mode . yafolding-mode)
  )
;; (add-hook 'prog-mode-hook
;;           (lambda () (yafolding-mode)))

(use-package json-mode
  :straight t
  :ensure t
  )

;; Compressed JSON sure is ugly and illegible; This solves that
;; problem.
(use-package json-reformat
  :straight t
  :ensure t
  :after json-mode
:init (setq json-reformat:indent-width 2))


(use-package tree-sitter
  :straight (tree-sitter :host github
                         :repo "ubolonton/emacs-tree-sitter"
                         :files ("lisp/*.el"))
  :ensure t
  :init (global-tree-sitter-mode)
  :hook ((js-mode . tree-sitter-hl-mode)
         (ruby-mode . tree-sitter-mode) ;; with tree-sitter-hl-mode, the buffer
                                        ;; is quite a bit busier with color, and
                                        ;; works against my asthetic preferences
         (typescript-mode . tree-sitter-hl-mode)
         (go-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :straight (tree-sitter-langs :type git :host github :repo "ubolonton/emacs-tree-sitter" :files ("langs/*.el" "langs/queries"))
  :ensure t)

(use-package undo-tree
  :diminish
  :bind (("C-c _" . undo-tree-visualize))
  :config
  (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map))

;; Add a gopher and gemini client
(use-package elpher
  :straight t
  :ensure t
  )

;; Adding format to git-commit-fill-column of 72 as best
;; practice.
(use-package magit
  :straight t
  :ensure t
  :after ivy

  :init (setq git-commit-fill-column 72)
  :bind (("H-g" . magit-status)
         ("C-M-g" . magit-status)))

(use-package forge
  :straight t
  :ensure t
  :after magit)

(use-package libgit
  :ensure t
  :straight t)

(use-package magit-libgit
  :ensure t
  :straight t
  :after (magit libgit))

(use-package keychain-environment
  :straight t
  :ensure t
  :config
  (keychain-refresh-environment))

(use-package password-generator
  :straight t
  :ensure t
  )

(use-package hungry-delete
  :straight t
  :ensure t
  :config (global-hungry-delete-mode))

;; Open svg files in xml-mode (instead of image rendering mode)
(add-to-list `auto-mode-alist
             '("\\.svg\\'" . xml-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
(with-eval-after-load "magit"
  (setq magit-completing-read-function 'ivy-completing-read))

;; With the time machine, travel back and forth through a files history
(use-package git-timemachine
  :straight t
  :ensure t
  )

(use-package git-gutter
  :straight t
  :ensure t
  :config (global-git-gutter-mode 't))

;; https://github.com/sshaw/git-link
;; `M-x git-link` to add the current URL to the kill ring
(use-package git-link
  :straight t
  :ensure t
  :config (setq git-link-use-commit t) ;; URL will be SHA instead of branch
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

  :bind (([C-s-down] . move-text-down)
         ([C-s-up] . move-text-up)))


;; Allow emacs to be the editor for textareas on a webpage.
;;
;; Note: This requires installing plugins in browsers
;;
(use-package atomic-chrome
  :straight t
  :ensure t
  )
(atomic-chrome-start-server)
(setq atomic-chrome-default-major-mode 'markdown-mode)

;; Some websites have aggressive JS which triggers when text is
;; entered to a textarea which can lead to bugs in combination with
;; AtomicChrome. There’s some websites where I regularly lose the text
;; that’s entered. While I’m editing, the textarea is updating, but on
;; C-c C-c, Emacs closes and the textarea is empty. For such cases,
;; I’m using this simple workaround: Copy the contents to clipboard
;; just before closing Emacs. So if the contents are lost, I can just
;; paste the text into the textarea. Not a perfect solution, but this
;; happens seldomly enough, that it’s good enough for me.
;;
(advice-add 'atomic-chrome-close-current-buffer
            :before
            '(lambda()
               (clipboard-kill-ring-save (point-min) (point-max))))


;; https://blog.sumtypeofway.com/posts/emacs-config.html
(defun jnf/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; I never want these.
(unbind-key "C-x C-f") ;; find-file-read-only
(unbind-key "C-x C-d") ;; list-directory
(unbind-key "C-z") ;; suspend-frame

(global-set-key (kbd "C-c f") 'jnf/copy-file-name-to-clipboard)
(global-set-key (kbd "M-n") 'jnf/copy-file-name-to-clipboard) ;; Deprecated
(global-set-key (kbd "C-c C-b") 'browse-web) ;; CTRL+C CTRL+b
(global-set-key (kbd "C-s-w") 'browse-url-at-point) ;; CTRL+CMD+w
(global-set-key (kbd "s-b") 'switch-to-buffer) ;; CMD+b
(global-set-key (kbd "C-s-b") 'switch-to-buffer-other-window) ;; CTRL+CMD+b
(setq browse-url-browser-function 'eww-browse-url)

(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "s-l") 'goto-line)
;; (global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-DEL") 'backward-kill-paragraph)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-w") 'kill-current-buffer)


;; https://blog.sumtypeofway.com/posts/emacs-config.html
(defun jnf/revert-to-two-windows ()
  "Delete all other windows and split it into two."
  (interactive)
  (delete-other-windows)
  (split-window-right))

(bind-key "C-x C-1" #'jnf/revert-to-two-windows)

(defun jnf/kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(global-set-key (kbd "C-w") 'jnf/kill-region-or-backward-word)

;; https://melpa.org/#/elfeed
;; (global-set-key (kbd "C-x r") 'elfeed)

(if (eq system-type 'darwin)
    (progn (add-to-list 'load-path "~/git/dotzshrc/emacs/darwin")
           (require 'emacs-config.el)))

(if (eq system-type 'gnu/linux)
    (progn (add-to-list 'load-path "~/git/dotzshrc/emacs/gnu-linux")
           (require 'emacs-config.el)))

;; https://blog.sumtypeofway.com/posts/emacs-config.html
;; Replaces the duplicate line function I once had
(use-package duplicate-thing
  :straight t
  :ensure t
  :init
  (defun my-duplicate-thing ()
    "Duplicate thing at point without changing the mark."
    (interactive)
    (save-mark-and-excursion (duplicate-thing 1)))
  :bind (("C-M-d" . my-duplicate-thing)))

(use-package whole-line-or-region
  :straight t
  :ensure t
  :config (whole-line-or-region-global-mode))

(use-package plantuml-mode
  :straight t
  :ensure t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN ORG mode configuration and concerns

(use-package org-sidebar
  :straight (org-sidebar :type git :host github :repo "alphapapa/org-sidebar")
  :ensure t
  :bind (
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c t" . org-toggle-link-display)
         ))

(setq org-directory "~/git/org")
(setq org-agenda-files (list "~/git/org"))
(setq org-default-notes-file (concat org-directory "/captured-notes.org"))

;; To make Org mode take care of versioning of attachments for you,
;; add the following to your Emacs config:
;;
(require 'org-attach-git)

;; I'm working through what templates I might want. This is a place holder.
(setq org-capture-templates
      '(
        ("c" "Write to Current Clock" plain (clock)
         "%?\n %a")
        ("m" "Meeting for Work" entry (file+datetree "~/git/org/agenda.org")
         "* MEETING %^{SUMMARY}\n\n  %^{ATTENDEES}p\n  %?\n")
        ("r" "Reading for Work" entry (file+datetree "~/git/org/agenda.org")
         "* TO-READ %^{SUBJECT} %u\n  %?\n")
        ("s" "Session" entry (file+headline "~/git/org/sessions.org" "Sessions")
         "* Session: %u %^{SUMMARY}\n\n  %^{ATTENDEES}p\n  %^{SYSTEM}p\n  %?\n")
        ("g" "Troubleshooting" entry (file+headline "~/git/org/troubleshooting.org" "Trouble Shooting")
         "* TODO %u Problem %^{SUMMARY}\n\n  %?\n  %a")
        ("t" "Task for Work" entry (file+datetree "~/git/org/agenda.org")
         "* TODO %?")
        ("u" "Unfiled Permanent > Bibliography" entry (file+headline "~/git/org/permanent/unfiled_bibliographic_cards.org" "Unfiled Bibliographic Cards")
         "* UNFILED %?\nEntered on %U")
        ("w" "Waiting for Work" entry (file+datetree "~/git/org/agenda.org")
         "* WAITING %^{SUMMARY}\n\n  %?\n")
        ))


;; Different key words and state machines help contextual the work.
;;
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE")
        (sequence "MEETING" "AGENDA" "|" "MINUTES")
        (sequence "UNFILED" "|" "FILED")
        (sequence "TO-READ" "READING" "|" "READ"))
      )

(use-package org-web-tools
  :ensure t
  :straight t)

(use-package org-d20
  :ensure t
  :straight t)


(defalias 'roll 'org-d20-roll-at-point)

(use-package org-bullets
  :straight t
  :hook (
         (org-mode . org-bullets-mode)
         (org-mode . turn-on-visual-line-mode)
  ))

;; (add-hook 'org-mode-hook #'toggle-word-wrap)
;; (add-hook 'org-mode-hook #'turn-on-visual-line-mode)

;; See
;; https://www.reddit.com/r/orgmode/comments/i6hl8b/image_preview_size_in_org_mode/
;; for further discussion
;;
;; One consideration is that the below setq should be called as part
;; of the `org-toggle-inline-images`
(setq org-image-actual-width (truncate (* (window-pixel-width) 0.8)))

(defun org-frames-toggle ()
  "Toggle the org-sidebar and org-sidebar-tree; Both are quite useful when working on org docs"
  (interactive)
  (org-sidebar-tree-toggle)
  (org-sidebar-toggle))
(defalias 'ot 'org-frames-toggle)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)))

;; For some reason, when I load emacs in daemon mode, the daemon
;; process is the process now renders the GET prompts for the
;; mini-buffer.  When I load the file interactively, I don't
;; experience the same problem.  So, until this resolves, I'll need to
;; load roam via an interactive command.
;; (global-set-key (kbd "<f11>") `(lambda ()
;;                                 (interactive)
;;                                 (require 'jnf-org-roam.el)
;;                                 ))
;;
;; With the latest update of org-roam, things again behavior
;; correctly.  Now I can just load org-roam as part of my day to day
(require 'jnf-org-roam.el)
(require 'jnf-spelling.el)
(require 'jnf-typopunct.el)
(require 'jnf-ruby.el)

(global-set-key (kbd "<f2>") `(
                               lambda ()
                               (interactive)
                               (find-file "~/git/org/agenda.org")))
(global-set-key (kbd "<f5>") `(lambda () (interactive)(find-file "~/git/org/troubleshooting.org")))
(global-set-key (kbd "<f12>") `(lambda () (interactive)(find-file "~/git/dotzshrc/symlinks/.emacs")))
(global-set-key (kbd "<f13>") `rss)

;; (require 'jnf-org-latex.el)

(use-package elfeed
  :ensure t
  :straight t
  :after org
  :bind (("C-x r" . jnf/elfeed-load-db-and-open)
         :map elfeed-search-mode-map
         ("q" . jnf/elfeed-save-db-and-bury))
  :config (elfeed-org)

  ;;
  ;; linking and capturing
  ;;

  (defun elfeed-link-title (entry)
    "Copy the entry title and URL as org link to the clipboard."
    (interactive)
    (let* ((link (elfeed-entry-link entry))
           (title (elfeed-entry-title entry))
           (titlelink (concat "[[" link "][" title "]]")))
      (when titlelink
        (kill-new titlelink)
        (x-set-selection 'PRIMARY titlelink)
        (message "Yanked: %s" titlelink))))

  ;; show mode

  (defun elfeed-show-quick-url-note ()
    "Fastest way to capture entry link to org agenda from elfeed show mode"
    (interactive)
    (elfeed-link-title elfeed-show-entry)
    (org-capture nil "u")
    (yank)
    ;; (org-capture-finalize)
    )

  (bind-keys :map elfeed-show-mode-map
             ("l" . elfeed-show-link-title)
             ("v" . elfeed-show-quick-url-note)))

;; A little bit of RSS beautification
(add-hook 'elfeed-show-mode-hook 'jnf/elfeed-visual)
(defun jnf/elfeed-visual ()
  "A method to turn on visual line mode and adjust text scale."
  (text-scale-set 2)
  (turn-on-visual-line-mode)
  )

;;write to disk when quiting
(defun jnf/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(defun jnf/elfeed-load-db-and-open ()
  "Load the elfeed db from disk before opening"
  (interactive)
  (elfeed)
  (elfeed-update)
  (elfeed-db-load)
  (elfeed-search-update--force))
(defalias 'rss 'jnf/elfeed-load-db-and-open)

(use-package elfeed-org
  :straight t
  :ensure t
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/git/org/elfeed.org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From https://karthinks.com/blog/lazy-elfeed/
(defun elfeed-search-show-entry-pre (&optional lines)
  "Returns a function to scroll forward or back in the Elfeed
  search results, displaying entries without switching to them."
  (lambda (times)
    (interactive "p")
    (forward-line (* times (or lines 0)))
    (recenter)
    (call-interactively #'elfeed-search-show-entry)
    (select-window (previous-window))
    (unless elfeed-search-remain-on-entry (forward-line -1))))
(eval-after-load 'elfeed-search
  '(define-key elfeed-search-mode-map (kbd "n") (elfeed-search-show-entry-pre +1)))
(eval-after-load 'elfeed-search
  '(define-key elfeed-search-mode-map (kbd "p") (elfeed-search-show-entry-pre -1)))
(eval-after-load 'elfeed-search
  '(define-key elfeed-search-mode-map (kbd "M-RET") (elfeed-search-show-entry-pre)))
;; End https://karthinks.com/blog/lazy-elfeed/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Uncomment to always launch org mode with a sidebar tree
;; (add-hook 'org-mode-hook #'org-sidebar-tree)

;; END ORG mode configuration and concerns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dotfiles--gc-on-last-frame-out-of-focus ()
  "GC if all frames are inactive."
  (if (seq-every-p #'null (mapcar #'frame-focus-state (frame-list)))
      (garbage-collect)))

(add-function :after after-focus-change-function
              #'dotfiles--gc-on-last-frame-out-of-focus)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
