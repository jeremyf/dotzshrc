;;; init.el --- Summary:
;;; -*- lexical-binding: t; -*-
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

;; I have additional files that I require in the emacs directory
(add-to-list 'load-path (expand-file-name "~/git/dotzshrc/emacs"))

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

(require 'jnf-display.el)
(require 'jnf-hydra.el)

(global-auto-revert-mode)

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
         ("C-x C-f" . counsel-find-file)
         ("C-x 8 RET" . counsel-unicode-char)
         ("<f4>" . counsel-bookmark)
         ("s-4" . counsel-bookmark)
         ("s-r" . counsel-recentf))
  :config (counsel-mode 1)
  (defalias 'recent 'counsel-recentf))
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; https://www.emacswiki.org/emacs/RecentFiles#h5o-1
;; Save recentf list every five minutes
(run-at-time nil (* 5 60) 'recentf-save-list)

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
  (ivy-rich-mode 1))

(use-package prescient
  :straight t)

(use-package ivy-prescient
  :straight t
  :init (setq prescient-filter-method '(literal fuzzy regexp initialism))
  :config (ivy-prescient-mode t))

(use-package company-prescient
  :straight t
  :config (company-prescient-mode t))

;; The silver searcher; I found ripgrep a bit nicer, but wait until
;; you try wgrep-ag
(use-package ag
  :straight t
  :after counsel
  :init
  ;; There are two paths into ag, I most often (like 99.9% of the
  ;; time) use the counsel-ag.  I want both ways into ag to be
  ;; similar.
  ;;
  ;; I've added "--hidden --ignore-dir .git" to both of the default
  ;; cases.
  (setq counsel-ag-base-command "ag --hidden --ignore-dir .git --vimgrep %s"
        ag-arguments (list "--smart-case" "--stats" "--hidden" "--ignore" ".git"))
  :bind (("C-c f" . counsel-ag)
         ("M-s-f" . counsel-ag))) ; Deprecated

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

;; M-o e to open counsel edit mode
(use-package counsel-edit-mode
  :straight (counsel-edit-mode :host github :type git :repo "tyler-dodge/counsel-edit-mode")
  :init (counsel-edit-mode-setup-ivy))

;; I have found this package quite "helpful"; When I want to know the
;; name of a function or key or variable, I can use the helpful
;; package.
(use-package helpful
  :straight t
  :pretty-hydra
  ((:title (with-material "help_outline" "Helpful Menus") :quit-key "q")
   ("Helpful"
    (("f" helpful-callable "callable")
     ("c" helpful-command "command")
     ("u" helpful-function "function")
     ("k" helpful-key "key")
     ("d" helpful-at-point "thing at point")
     ("v" helpful-variable "variable"))))
  :bind ("C-h" . helpful-hydra/body)
  :init (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

;; https://docs.projectile.mx/en/latest/
;;
;; Helpful for understanding the likely bounds of directory structure
(use-package projectile
  :straight t
  :config (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
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

;; A window manager for emacs, allowing fast toggles between windows
;; as well as opening or moving those windows.
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :straight t
  :bind (("M-o" . ace-window)))

;; Browse remote git repositories; A prior package I used limited the
;; behavior to Github. This one is bitbucket, github, sourcehut,
;; gitlab, etc.
(use-package browse-at-remote
  :straight t)

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
 '(safe-local-variable-values '((encoding . utf-8)))
 '(show-paren-mode t)
 '(typopunct-buffer-language 'english)
 '(use-package-always-ensure t))

(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; Favor neotree over sr-speedbar
(use-package neotree
  :straight t
  :after (all-the-icons)
  :init (setq-default neo-window-width 36)
  :config (global-set-key [f8] 'neotree-toggle))
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(use-package emmet-mode
  :straight t
  :bind (("C-c C-e" . emmet-expand-yas ))
  :hook ((sgml-mode . emmet-mode)
         (html-mode . emmet-mode)
         (css-mode . emmet-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Begin EPUB
;; https://depp.brause.cc/nov.el/
(use-package nov
  :straight t
  :mode (("\\.epub\\'" . nov-mode)))

(use-package justify-kp
  :straight (justify-kp :host github :type git :repo "Fuco1/justify-kp"))

(use-package visual-fill-column
  :straight t)

(setq-default split-window-preferred-function 'visual-fill-column-split-window-sensibly)

(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "ETBembo"
                           :height 1.3))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

(setq nov-text-width 80)
(setq nov-text-width t)
(setq visual-fill-column-center-text t)
(add-hook 'nov-mode-hook 'visual-line-mode)
(add-hook 'nov-mode-hook 'visual-fill-column-mode)

(defun my-nov-window-configuration-change-hook ()
  (my-nov-post-html-render-hook)
  (remove-hook 'window-configuration-change-hook
               'my-nov-window-configuration-change-hook
               t))

(defun my-nov-post-html-render-hook ()
  (if (get-buffer-window)
      (let ((max-width (pj-line-width))
            buffer-read-only)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (not (looking-at "^[[:space:]]*$"))
              (goto-char (line-end-position))
              (when (> (shr-pixel-column) max-width)
                (goto-char (line-beginning-position))
                (pj-justify)))
            (forward-line 1))))
    (add-hook 'window-configuration-change-hook
              'my-nov-window-configuration-change-hook
              nil t)))

(add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook)
;; End EPUB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/jrblevin/markdown-mode/
(use-package markdown-mode
  :straight t
  ;; :hook ((markdown-mode . turn-on-visual-line-mode))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/pandoc"))

;; https://github.com/AdamNiederer/vue-mode
(use-package vue-mode
  :straight t
  :mode (("\\.vue\\'" . vue-mode)))

(use-package web-mode
  :straight t
  :config (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN Typescript
(use-package typescript-mode
  :straight t
  :mode (("\\.ts\\'" . typescript-mode)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :straight t
  :bind (:map typescript-mode-map ("C-l ." . 'tide-references))
  :hook ((typescript-mode . setup-tide-mode)))

;; END Typescript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode
  :straight t)

(use-package plantuml-mode
  :config (setq plantuml-executable-path "/usr/local/bin/plantuml"
                plantuml-default-exec-mode 'executable
                org-plantuml-executable-path "/usr/local/bin/plantuml"
                org-plantuml-exec-mode 'executable)
  :mode (("\\.plantuml\\'" . plantuml-mode))
  :straight t)

;; That letter is the beginning of a word. Narrow results from there.
(use-package avy
  :straight t)

(use-package json-mode
  :straight t)

;; Compressed JSON sure is ugly and illegible; This solves that
;; problem.
(use-package json-reformat
  :straight t
  :after json-mode
  :init (setq json-reformat:indent-width 2))

;; Add a gopher and gemini client
(use-package elpher
  :straight t)
(use-package keychain-environment
  :straight t
  :config
  (keychain-refresh-environment))

(use-package password-generator
  :straight t)


;; Open svg files in xml-mode (instead of image rendering mode)
(add-to-list `auto-mode-alist '("\\.svg\\'" . xml-mode))

;; I never want these.
(unbind-key "C-x C-d") ;; list-directory
(global-set-key (kbd "C-x C-d") 'dired)
(unbind-key "C-z") ;; suspend-frame


(global-set-key (kbd "C-s-w") 'browse-url-at-point) ;; CTRL+CMD+w
(global-set-key (kbd "s-b") 'switch-to-buffer) ;; CMD+b
(global-set-key (kbd "C-s-b") 'switch-to-buffer-other-window) ;; CTRL+CMD+b
(setq browse-url-browser-function 'eww-browse-url)

;; https://blog.sumtypeofway.com/posts/emacs-config.html
(defun jnf/revert-to-two-windows ()
  "Delete all other windows and split it into two."
  (interactive)
  (delete-other-windows)
  (split-window-right))

(bind-key "C-x C-1" #'jnf/revert-to-two-windows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN frame and window quick setup
(defun gk-layouts-3col ()
  "Three column layout.

Tries to preserve the order of window buffers and active window."
  (interactive)
  ;; Record active window buffer.
  (let ((cbuf (current-buffer)))
    ;; Switch to leftmost window.
    (ignore-errors (cl-loop do (windmove-left)))
    (let ((buffers
           (mapcar #'window-buffer (-take 3 (window-list))))
          (width (/ (frame-width) 3)))
      (delete-other-windows)
      (split-window-horizontally width)
      (other-window 1)
      (split-window-horizontally)k
      (other-window -1)
      (dolist (b buffers)
        (switch-to-buffer b)
        (other-window 1)))
    ;; Switch to previously visible buffer’s window.
    (select-window (get-buffer-window cbuf))))


(defun gk-layouts-main-and-sidekicks ()
  "One horizontal split, the right window split in two.

Tries to preserve the order of window buffers and active window."
  (interactive)
  ;; Record active window buffer.
  (let ((cbuf (current-buffer)))
    ;; Switch to leftmost window.
    (ignore-errors (cl-loop do (windmove-left)))
    (let ((buffers
           (mapcar #'window-buffer (-take 3 (window-list)))))
      (delete-other-windows)
      (split-window-horizontally)
      (other-window 1)
      (split-window-vertically)
      (other-window -1)
      (dolist (b buffers)
        (switch-to-buffer b)
        (other-window 1)))
    ;; Switch to previously visible buffer’s window.
    (select-window (get-buffer-window cbuf))))

(bind-key "C-x \\" #'gk-layouts-main-and-sidekicks)


;; END frame and window quick setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (eq system-type 'darwin)
    (progn (add-to-list 'load-path "~/git/dotzshrc/emacs/darwin")
           (require 'emacs-config.el)))

(if (eq system-type 'gnu/linux)
    (progn (add-to-list 'load-path "~/git/dotzshrc/emacs/gnu-linux")
           (require 'emacs-config.el)))

;; This is a common function that I've used in other text editors.
;; It's a simple stitch together of sort-lines and
;; delete-duplicate-lines.
(defun sort-unique-lines (reverse beg end &optional adjacent keep-blanks interactive)
  "Sort lines and delete duplicates.
By default the sort is lexigraphically ascending.  To sort as
descending set REVERSE to non-nil.  Specify BEG and END for the
bounds of sorting.  By default, this is the selected region.

I've included ADJACENT, KEEP-BLANKS, and INTERACTIVE so I can
echo the method signature of `'delete-duplicate-lines`"
  (interactive "P\nr")
  (sort-lines reverse beg end)
  (delete-duplicate-lines beg end reverse adjacent keep-blanks interactive))

(require 'jnf-in-buffer.el)
(require 'jnf-swiper.el)
(require 'jnf-org.el)
(require 'jnf-basic-config.el)
(require 'jnf-git.el)
(require 'jnf-spelling.el)
(require 'jnf-typopunct.el)
(require 'jnf-ruby.el)
(require 'jnf-lsp-mode.el)
(require 'jnf-blogging.el)
(require 'jnf-tabs.el)
(require 'jnf-stars-without-number.el)
(require 'jnf-elfeed.el)

;; Consider for publishing: https://github.com/rnkn/binder
;; Consider as replacement for org-roam: https://github.com/EFLS/zetteldeft
;; And https://github.com/zzamboni/ox-leanpub

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Begin burly/bufly
;;

;; (use-package burly :straight (burly :type git :host github :repo "alphapapa/burly.el"))
;; (use-package bufly :straight (bufly :type git :host github :repo "alphapapa/burly.el"))

;; (use-package bufler
;;   :straight (bufler :type git :host github :repo "alphapapa/bufler.el"
;;                     :files (:defaults (:exclude "helm-bufler.el"))))

;; End burly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dotfiles--gc-on-last-frame-out-of-focus ()
  "GC if all frames are inactive."
  (if (seq-every-p #'null (mapcar #'frame-focus-state (frame-list)))
      (garbage-collect)))

(add-function :after after-focus-change-function
              #'dotfiles--gc-on-last-frame-out-of-focus)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
