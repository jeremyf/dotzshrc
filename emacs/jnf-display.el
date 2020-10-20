;;; jnf-display.el --- Summary
;;
;;; Commentary:
;;
;;  Some basic display options:
;;
;;  * Themes
;;  * Colors
;;  * Cursor type
;;  * Icons
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I'm just going to trust themes
(setq custom-safe-themes t)

(add-to-list 'default-frame-alist '(font . "JetBrains Mono 14" ))
(set-face-attribute 'default t :font "JetBrains Mono 14" )

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
        modus-operandi-theme-links 'faint
        modus-operandi-theme-comments 'yellow
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
  :init
  (setq modus-vivendi-theme-org-blocks 'rainbow)
  (setq modus-vivendi-theme-completions 'opinionated)
  (setq modus-vivendi-theme-fringes 'subtle)
  (setq modus-vivendi-theme-scale-headings t
        modus-vivendi-theme-slanted-constructs t
        modus-vivendi-theme-bold-constructs t
        modus-vivendi-theme-faint-syntax nil
        modus-vivendi-theme-intense-hl-line nil
        modus-vivendi-theme-mode-line 'moody
        modus-vivendi-theme-completions 'opinionated
        modus-vivendi-theme-links 'faint
        modus-vivendi-theme-comments 'yellow
        modus-vivendi-theme-intense-paren-match t)
  :straight t)

(defun modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn
        (disable-theme 'modus-operandi)
        (custom-set-faces
         `(font-lock-variable-name-face ((t(:foreground ,(cdr(assoc "blue" modus-vivendi-theme-default-colors-alist))))))
         `(font-lock-string-face ((t(:foreground ,(cdr(assoc "green" modus-vivendi-theme-default-colors-alist)) :weight bold))))
	 )
        (load-theme 'modus-vivendi t))
    (disable-theme 'modus-vivendi)
    (custom-set-faces
     `(font-lock-variable-name-face ((t(:foreground ,(cdr(assoc "blue" modus-operandi-theme-default-colors-alist))
                                                    :background "#e6edff"))))
     `(font-lock-string-face ((t(:foreground ,(cdr(assoc "green" modus-operandi-theme-default-colors-alist))
                                             :weight bold
                                             :background "#f2f7ed")))))
    (load-theme 'modus-operandi t)))

(global-set-key (kbd "<s-clear>") 'modus-themes-toggle)

(blink-cursor-mode t)
;; Doing a bit of configuration of my cursors
(setq-default cursor-type 'bar)

;; Main typeface, I'm toggling between "JetBrains Mono" and "Hack"
(set-face-attribute 'default nil :family "JetBrains Mono" :height 140)
;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "Times" :height 1.0)
;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 1.0)


;; Nice for neotree
(use-package all-the-icons
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package spaceline
  :straight t)

(use-package spaceline-all-the-icons
  :straight t
  :after spaceline
  :config (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-neotree))

;; Adding smartparens options
(use-package smartparens
  :straight t
  :config (smartparens-strict-mode 1)
  (smartparens-global-mode 1))

;; Core APIs of tree-sitter
(use-package tsc
	     :straight (tsc :host github
			    :repo "ubolonton/emacs-tree-sitter"
			    :files ("core/*.el")))

;; Base framework, syntax highlighting.
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

;; Language bundle.
(use-package tree-sitter-langs
  :straight (tree-sitter-langs :type git
                               :host github
                               :repo "ubolonton/emacs-tree-sitter"
                               :files ("langs/*.el" "langs/queries")))
(provide 'jnf-display.el)
;;; jnf-display.el ends here
