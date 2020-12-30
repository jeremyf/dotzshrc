;;; jnf-in-buffer.el --- Summary
;;
;;; Commentary:
;;
;;  Packages that greatly assist with in-buffer editing:
;;
;;  * navigation
;;  * case manipulation
;;  * region highlighting (or compression)
;;
;;  As I've spent time thinking about this, it's hard to separate.
;;  But there's some kind of logic.  Namely, I'm operating only on
;;  this buffer.  Not navigating between buffers.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Write "kill" command inputs to disk
(use-package savekill
  :straight t)

(use-package which-key
  :config (which-key-mode)
  :straight t)

;; I don't use a lot of folding, this allows me to type C-RET and fold
;; the current block.  There's more it can do but for now that's
;; enough
(use-package yafolding
  :straight t
  :hook (prog-mode . yafolding-mode))

;; Using Hippie expand, I toggle through words already referenced.
(setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                         try-expand-dabbrev
                                         try-expand-list
                                         try-expand-all-abbrevs
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol
                                         ))
(global-set-key (kbd "M-SPC") 'hippie-expand)

;; Expand or contract point/region to next logical element.
(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region)))

;; This package allows me to toggle between different string cases.
;;
;; - HELLO WORLD
;; - HelloWorld
;; - helloWorld
;; - hello-world
;; - Hello_World
;; - hellow_world
;; - HELLO_WORLD
(use-package string-inflection
  :bind (("H-u" . string-inflection-all-cycle)
         ("C-M-u" . string-inflection-all-cycle)) ;; CTRL+OPT+u
  :straight (string-inflection :type git
                               :host github
                               :repo "akicho8/string-inflection"))

;; Allow to work with multipe cursors
;; https://melpa.org/#/multiple-cursors Aside from the
;; set-rectangular-region-anchor, there are several additional
;; features to practice
(use-package multiple-cursors
  :bind (("H-SPC" . set-rectangular-region-anchor)
         ("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)) ;; CTRL+CMD+c
  :straight t)

;; C-; to select current symbol and all matches; Then edit at multiple points.
(use-package iedit
  :straight t)

;; Not quite sexp navigation, but something alternative.  For vertical
;; movement, it looks at an empty line and moves there.  For
;; horizontal movement, moves between two spaces.
(use-package spatial-navigate
  :straight (spatial-navigate :type git
                              :host gitlab
                              :repo "ideasman42/emacs-spatial-navigate")
  :bind (("<M-s-up>" . #'spatial-navigate-backward-vertical-bar)
         ("<M-s-down>" . #'spatial-navigate-forward-vertical-bar)
         ("<M-s-left>" . #'spatial-navigate-backward-horizontal-bar)
         ("<M-s-right>" . #'spatial-navigate-forward-horizontal-bar)))

;; C-a goes to the first non-whitepsace character on the line. Type it
;; again, and go to the beginning of the line.
(use-package crux
  :straight t
  :bind (("C-a" . crux-move-beginning-of-line)
         ("<f9>" . crux-kill-other-buffers)))

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

;; Provides a UI for undo trees.  I'm not certain what I want to do with this.
(use-package undo-tree
  :diminish
  :bind (("C-z" . undo)
         ("C-S-z" . undo-tree-redo))
  :config
  (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map))


;; Delete multiple spaces
(use-package hungry-delete
  :straight t
  :config (global-hungry-delete-mode))

;; Adding ability to move lines up and down
(use-package move-text
  :straight t
  :bind (([C-s-down] . move-text-down)
         ([C-s-up] . move-text-up)))

(use-package company
  :straight t
  :diminish
  :bind (("C-." . #'company-capf)
         (:map company-active-map (("<tab>" . 'company-complete-selection ))))
  :hook (after-init . global-company-mode)
  :custom
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length 3)
  (company-tooltip-idle-delay 0.25 "Faster!")
  (company-async-timeout 20 "Some requests can take a long time. That's fine.")
  :config

  ;; Use the numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9))))

;; In auto-complete, display any associated help with the completion options.
(use-package company-quickhelp
  :straight t
  :bind (:map company-active-map ("C-c h" . 'company-quickhelp-manual-begin))
  :config (company-quickhelp-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN EMOJIS
;;
;; Add emoji handling
(use-package emojify
  :straight t
  :hook (text-mode . emojify-mode))

(use-package unicode-fonts
  :straight t
  :ensure t
  :config (unicode-fonts-setup))

(use-package company-emoji
  :straight (company-emoji :type git :host github :repo "dunn/company-emoji" :branch "trunk")
  :config (add-to-list 'company-backends 'company-emoji))

(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

;; For when Emacs is started in GUI mode:
(--set-emoji-font nil)
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions '--set-emoji-font)
;;
;; END EMOJIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jnf/kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(global-set-key (kbd "C-w") 'jnf/kill-region-or-backward-word)

;; A rather convenient snippet manager.  When you create a snippet, it
;; understands the mode you're in and puts the snippet in the right
;; place.
(use-package yasnippet
  :straight t
  :after company
  :init (setq yas-snippet-dirs '("~/git/dotzshrc/emacs/snippets"))
  (yas-global-mode 1))

(defalias 'tp 'transpose-pagraphs)
(defalias 'ts 'transpose-sentence)


;; https://blog.sumtypeofway.com/posts/emacs-config.html
(defun jnf/nab-file-name-to-clipboard ()
  "Nab, I mean copy, the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
(global-set-key (kbd "C-c n") 'jnf/nab-file-name-to-clipboard)

(use-package whole-line-or-region
  :straight t
  :config (whole-line-or-region-global-mode))


;; https://blog.sumtypeofway.com/posts/emacs-config.html
;; Replaces the duplicate line function I once had
(use-package duplicate-thing
  :straight t
  :init
  (defun my-duplicate-thing ()
    "Duplicate thing at point without changing the mark."
    (interactive)
    (save-mark-and-excursion (duplicate-thing 1)))
  :bind (("C-M-d" . my-duplicate-thing)
         ("C-c d" . my-duplicate-thing)))


(defun sp-wrap-double-quote ()
  "Wrap following sexp in double quotes (e.g. `\"')."
  (interactive)
  (sp-wrap-with-pair  "\""))

(defun sp-wrap-single-quote ()
  "Wrap following sexp in double quotes (e.g. `\'')."
  (interactive)
  (sp-wrap-with-pair  "'"))

(global-set-key (kbd "C-\"") 'sp-wrap-double-quote)
(global-set-key (kbd "C-'") 'sp-wrap-single-quote)
(global-set-key (kbd "M-[") 'sp-wrap-square)
(global-set-key (kbd "M-]") 'sp-wrap-round)
(global-set-key (kbd "M-{") 'sp-wrap-curly)
(global-set-key (kbd "<f5>") 'eval-region)
(global-set-key (kbd "M-DEL") 'backward-kill-paragraph)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-w") 'kill-current-buffer)
(global-set-key (kbd "<f3>") 'counsel-imenu)
(global-set-key (kbd "s-3") 'counsel-imenu)

(provide 'jnf-in-buffer.el)
;;; jnf-in-buffer.el ends here
