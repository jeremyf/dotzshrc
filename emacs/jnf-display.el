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
   ;; Main typeface
  (set-face-attribute 'default nil :family "Hack" :height 130)
  ;; Proportionately spaced typeface
  (set-face-attribute 'variable-pitch nil :family "FiraGO" :height 1.0)
  ;; Monospaced typeface
  (set-face-attribute 'fixed-pitch nil :family "Hack" :height 1.0)
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
