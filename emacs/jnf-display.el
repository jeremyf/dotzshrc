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
  (setq modus-operandi-theme-slanted-constructs t
        modus-operandi-theme-bold-constructs t
        modus-operandi-theme-fringes 'subtle
        modus-operandi-theme-mode-line '3d
        modus-operandi-theme-syntax 'yellow-comments-green-strings
        modus-operandi-theme-intense-paren-match t
        modus-operandi-theme-links 'faint
        modus-operandi-theme-prompts 'intense
        modus-operandi-theme-completions 'opinionated
        modus-operandi-theme-diffs 'desaturated
        modus-operandi-theme-org-blocks 'rainbow
        modus-operandi-theme-scale-headings t)
  :config
  ;; To determine the face at point run kbd "C-u C-x =", the "C-x ="
  ;; is 'what-cursor-position, the prefix of C-u indicates to render
  ;; the detailed version of 'what-cursor-position
  ;; (custom-set-faces
  ;;  `(font-lock-variable-name-face ((t(:foreground ,(cdr(assoc "blue" modus-operandi-theme-default-colors-alist))
  ;;                                    :background "#e6edff"))))
  ;;  `(font-lock-string-face ((t(:foreground ,(cdr(assoc "green" modus-operandi-theme-default-colors-alist))
  ;;                                          :weight bold
  ;;                                          :background "#f2f7ed"))))
  ;;  )
  (defadvice load-theme (before theme-dont-propagate activate)
    "Disable theme before loading new one."
    (mapc #'disable-theme custom-enabled-themes))

  (load-theme 'modus-operandi t))

(use-package modus-vivendi-theme
  :init
  (setq modus-vivendi-theme-slanted-constructs t
        modus-vivendi-theme-bold-constructs t
        modus-vivendi-theme-fringes 'subtle
        modus-vivendi-theme-mode-line '3d
        modus-vivendi-theme-syntax 'yellow-comments-green-strings
        modus-vivendi-theme-intense-paren-match t
        modus-vivendi-theme-links 'faint
        modus-vivendi-theme-prompts 'intense
        modus-vivendi-theme-completions 'opinionated
        modus-vivendi-theme-diffs 'desaturated
        modus-vivendi-theme-org-blocks 'rainbow
        modus-vivendi-theme-scale-headings t)
  :straight t)

;; At present, I have not figured out enough about Emacs to remove the
;; duplication present in the use-package modus-operandi-them and the
;; below font-lock configuration.
;;
;; Note, I have not added font lock declarations to my
;; modus-vivendi-theme, as I default to modus-operandi and use the
;; toggle below to switch to vivendi.  Were I to default to vivendi,
;; I'd need to make some updates to the config.
(defun modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn
        (disable-theme 'modus-operandi)
        (load-theme 'modus-vivendi t)
        (set-background-color "#172637") ;; Background color for dark theme TakeOnRules.com
        )
    (disable-theme 'modus-vivendi)
    (load-theme 'modus-operandi t)
    (set-background-color "#fffff8") ;; Background color for light theme TakeOnRules.com
    ))

(global-set-key (kbd "<s-clear>") 'modus-themes-toggle)
(global-set-key (kbd "s-9") 'modus-themes-toggle)

;; I really like the background colors of my blog (light and dark
;; mode).  I'm using this to over-write the background colors.  I
;; don't not believe this to be the recommended way for Modus themes,
;; but it's what I've found to work.
;;
;; As the theme continues to move toward v1.0, I'd imagine more clear
;; instructions will emerge.  It turns out, I need more Elisp practice.
(add-to-list 'default-frame-alist '(background-color . "#fffff8"))
(set-background-color "#fffff8")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN BLOCK
;;
;; With a quick bit of testing, it appears that the following
;; set-face-attribute declarations should be made after the theme
;; declarations.  When the following statements were declared before
;; the themes, and I toggled my theme, the font changed to something
;; unexpected.  With them declared after, I keep the fonts between
;; toggles.
;;
;; Main typeface, I'm toggling between "JetBrains Mono" and "Hack"
(set-face-attribute 'default nil :family "JetBrains Mono" :height 140)
;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "Times" :height 1.0)
;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 1.0)
;;
;; END BLOCK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(blink-cursor-mode t)
;; Doing a bit of configuration of my cursors
(setq-default cursor-type 'bar)

;; Nice for neotree
(use-package all-the-icons
  :straight t)

;; Incorporates file icons with file listings of dired
(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; A nice looking modeline enhancement
(use-package spaceline
  :straight t)

;; Add some visual flair to the modeline enhancements
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

(provide 'jnf-display.el)
;;; jnf-display.el ends here
