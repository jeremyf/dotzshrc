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

;; I saw that straight loaded use-package to take advantage of the
;; use-package syntax which is often how things are documented.
(straight-use-package 'use-package)

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


;; I'm just going to trust themes
(setq custom-safe-themes t)

(use-package modus-operandi-theme
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
  :straight t)

(defun modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn
        (disable-theme 'modus-operandi)
        (load-theme 'modus-vivendi t))
    (disable-theme 'modus-vivendi)
    (load-theme 'modus-operandi t)))

;; Doing a bit of configuration of my cursors
(setq-default cursor-type 'bar)
(set-cursor-color "#44B4CC") ;; The text color of my
(blink-cursor-mode t)

;; Write "kill" command inputs to disk
(use-package savekill
  :straight t)

;; https://oremacs.com/swiper/
;; Note: I've set all searches to use fuzzy regex
(use-package ivy
  :straight t
  :after avy
  :diminish (ivy-mode . "")
  :bind (("C-c C-r" . ivy-resume))
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
  :init (setq ivy-use-selectable-prompt t)
  (setq search-default-mode #'char-fold-to-regexp)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config (counsel-mode 1))
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(defalias 'recent 'counsel-recentf)

(use-package all-the-icons-ivy-rich
  :straight t
  :after (ivy counsel counsel-projectile)
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
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
  :bind (("C-s" . swiper)))

(use-package prescient
  :straight t)

(use-package ivy-prescient
  :straight t
  :init (setq prescient-filter-method '(literal fuzzy regexp initialism))
  :config (ivy-prescient-mode t))

(use-package expand-region
  :straight t
  :bind (
         ("C-=" . er/expand-region)
         ("C-+" . er/contract-region)
         ))

;; The silver searcher; I found ripgrep a bit nicer, but wait until
;; you try wgrep-ag
(use-package ag
  :straight t
  :after counsel
  :bind (("M-s-f" . counsel-ag)))

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
  :straight t
  :hook (ag-mode . wgrep-ag-setup)
  :after ag)


;; I have found this package quite "helpful"; When I want to know the
;; name of a function or key or variable, I can use the helpful
;; package.
(use-package helpful
  :straight t
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
  :after projectile
  :bind ("s-t" . counsel-projectile-find-file)) ; CMD+t, which I carry over from Textmate


(use-package company
  :straight t
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
  :straight (string-inflection :type git
                               :host github
                               :repo "akicho8/string-inflection")
  :bind (("H-u" . string-inflection-all-cycle)
         ("C-M-u" . string-inflection-all-cycle))) ;; CTRL+OPT+u

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

;; C-; to select current symbol and all matches; Then edit at multiple points.
(use-package iedit
  :straight t)

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
  :straight t)

(use-package spaceline-all-the-icons
  :straight t
  :after spaceline
  :config (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-neotree))

;; A window manager for emacs, allowing fast toggles between windows
;; as well as opening or moving those windows.
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :straight t
  :bind (("M-o" . ace-window)))


;; "The long-awaited Emacs 27 support for native tabs is shaky, both
;; visually and in terms of functionality. As such, centaur-tabs is
;; the best way to simulate a conventional tabs setup, in which tab
;; sets are grouped by the toplevel project working directory."
;; https://blog.sumtypeofway.com/posts/emacs-config.html
(use-package centaur-tabs
  :straight t
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
  :bind (("s-{" . #'centaur-tabs-backward)
         ("s-}" . #'centaur-tabs-forward)
         ("C-c C-5". #'centaur-tabs-extract-window-to-new-frame)
         ([s-up] . #'centaur-tabs-backward-group)
         ([s-down] . #'centaur-tabs-forward-group)
         ("C-s-t" . #'centaur-tabs-counsel-switch-group)
         ("C-c C-o" . #'centaur-tabs-open-directory-in-external-application)))

(use-package spatial-navigate
  :straight (spatial-navigate :type git
                              :host gitlab
                              :repo "ideasman42/emacs-spatial-navigate")
  :bind (("<M-s-up>" . #'spatial-navigate-backward-vertical-bar)
         ("<M-s-down>" . #'spatial-navigate-forward-vertical-bar)
         ("<M-s-left>" . #'spatial-navigate-backward-horizontal-bar)
         ("<M-s-right>" . #'spatial-navigate-forward-horizontal-bar)))


;; Adding smartparens options
(use-package smartparens
  :straight t
  :config (smartparens-strict-mode 1)
  (smartparens-global-mode 1))

;; A rather convenient snippet manager.  When you create a snippet, it
;; understands the mode you're in and puts the snippet in the right
;; place.
(use-package yasnippets
  :straight (yasnippets :type git
                        :host github
                        :repo "joaotavora/yasnippet")
  :after company
  :config (push 'company-yasnippet company-backends)
  :bind (("C-c C-e" . yas-expand))
  :init (setq yas-snippet-dirs '("~/git/dotzshrc/emacs/snippets"))
  (yas-global-mode 1))

(defalias 'tp 'transpose-pagraphs)
(defalias 'ts 'transpose-sentence)

;; Browse remote git repositories; A prior package I used limited the
;; behavior to Github. This one is bitbucket, github, sourcehut,
;; gitlab, etc.
(use-package browse-at-remote
  :straight t
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
 '(show-paren-mode t)
 '(use-package-always-ensure t))

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
  :bind (("C-a" . crux-move-beginning-of-line)
         ("<f9>" . crux-kill-other-buffers)))

;; Nice for neotree
(use-package all-the-icons
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; Favor neotree over sr-speedbar
(use-package neotree
  :straight t
  :init (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq-default neo-window-width 36)
  :config (global-set-key [f8] 'neotree-toggle))


;; Whitespace hygene package.  The author's documentation and
;; commentary echoes my sentiments
(use-package ethan-wspace
  :straight t
  :hook (before-save . delete-trailing-whitespace)
  :init (setq-default mode-require-final-newline nil)
  :config (global-ethan-wspace-mode 1))

;; A package that is a bit of the inverse of 'fill-paragraph
;; (e.g. M-q).
(use-package unfill
  :straight t)

(use-package emmet-mode
  :straight t
  :hook ((sgml-mode . emmet-mode)
         (html-mode . emmet-mode)
         (css-mode . emmet-mode)))

;; https://github.com/jrblevin/markdown-mode/
(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/pandoc"))

(use-package yaml-mode
  :straight t)

(use-package plantuml-mode
  :straight t)

;; That letter is the beginning of a word. Narrow results from there.
(use-package avy
  :straight t)

;; I don't use a lot of folding, this allows me to type C-RET and fold
;; the current block.  There's more it can do but for now that's
;; enough
(use-package yafolding
  :straight t
  :hook (prog-mode . yafolding-mode))

(use-package json-mode
  :straight t)

;; Compressed JSON sure is ugly and illegible; This solves that
;; problem.
(use-package json-reformat
  :straight t
  :after json-mode
  :init (setq json-reformat:indent-width 2))


(use-package tree-sitter
  :straight (tree-sitter :host github
                         :repo "ubolonton/emacs-tree-sitter"
                         :files ("lisp/*.el"))
  :init (global-tree-sitter-mode)
  :hook ((js-mode . tree-sitter-hl-mode)
         (ruby-mode . tree-sitter-mode) ;; with tree-sitter-hl-mode, the buffer
                                        ;; is quite a bit busier with color, and
                                        ;; works against my asthetic preferences
         (typescript-mode . tree-sitter-hl-mode)
         (go-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :straight (tree-sitter-langs :type git
                               :host github
                               :repo "ubolonton/emacs-tree-sitter"
                               :files ("langs/*.el" "langs/queries")))

(use-package undo-tree
  :diminish
  :bind (("C-c _" . undo-tree-visualize))
  :config
  (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map))

;; Add a gopher and gemini client
(use-package elpher
  :straight t)
(use-package keychain-environment
  :straight t
  :config
  (keychain-refresh-environment))

(use-package password-generator
  :straight t)

(use-package hungry-delete
  :straight t
  :config (global-hungry-delete-mode))

;; Open svg files in xml-mode (instead of image rendering mode)
(add-to-list `auto-mode-alist
             '("\\.svg\\'" . xml-mode))


;; Adding ability to move lines up and down
(use-package move-text
  :straight t
  :bind (([C-s-down] . move-text-down)
         ([C-s-up] . move-text-up)))


;; Allow emacs to be the editor for textareas on a webpage.
;;
;; Note: This requires installing plugins in browsers
;;
(use-package atomic-chrome
  :straight t)
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
  :init
  (defun my-duplicate-thing ()
    "Duplicate thing at point without changing the mark."
    (interactive)
    (save-mark-and-excursion (duplicate-thing 1)))
  :bind (("C-M-d" . my-duplicate-thing)))

(use-package whole-line-or-region
  :straight t
  :config (whole-line-or-region-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN ORG mode configuration and concerns

(use-package org-sidebar
  :straight (org-sidebar :type git
                         :host github
                         :repo "alphapapa/org-sidebar")
  :bind (
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c t" . org-toggle-link-display)))

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
  :straight t)

(use-package org-d20
  :straight t)


(defalias 'roll 'org-d20-roll-at-point)

(use-package org-bullets
  :straight t
  :hook ((org-mode . org-bullets-mode)
         (org-mode . turn-on-visual-line-mode)))

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
(require 'jnf-git.el)
(require 'jnf-spelling.el)
(require 'jnf-typopunct.el)
(require 'jnf-ruby.el)
(require 'jnf-elfeed.el)

(global-set-key (kbd "<f2>") `(
                               lambda ()
                               (interactive)
                               (find-file "~/git/org/agenda.org")))
(global-set-key (kbd "<f5>") `(lambda () (interactive)(find-file "~/git/org/troubleshooting.org")))
(global-set-key (kbd "<f12>") `(lambda () (interactive)(find-file "~/git/dotzshrc/symlinks/.emacs")))

;; (require 'jnf-org-latex.el)



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
