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
  :config
  (defadvice load-theme (before theme-dont-propagate activate)
    "Disable theme before loading new one."
    (mapc #'disable-theme custom-enabled-themes)))

(use-package modus-vivendi-theme
  :straight t)

(defmacro modus-themes-format-sexp (sexp &rest objects)
  "A macro to evaluate the modus themes configuration."
  `(eval (read (format ,(format "%S" sexp) ,@objects))))

;; This `dolist' keeps synchronized the `modus-operandi' and
;; `modus-vivendi'.
(dolist (theme '("operandi" "vivendi"))
  (modus-themes-format-sexp
   (defun modus-%1$s-theme-load ()
     (setq modus-%1$s-theme-slanted-constructs t
           modus-%1$s-theme-bold-constructs t
           modus-%1$s-theme-fringes 'subtle
           modus-%1$s-theme-mode-line '3d
           modus-%1$s-theme-syntax 'alt-syntax-yellow-comments
           modus-%1$s-theme-intense-hl-line nil
           modus-%1$s-theme-intense-paren-match t
           modus-%1$s-theme-links 'faint
           modus-%1$s-theme-no-mixed-fonts nil
           modus-%1$s-theme-prompts 'intense
           modus-%1$s-theme-completions 'opinionated
           modus-%1$s-theme-diffs 'desaturated
           modus-%1$s-theme-org-blocks 'rainbow
           modus-%1$s-theme-variable-pitch-headings nil
           modus-%1$s-theme-scale-headings t
           modus-%1$s-theme-scale-1 1.1
           modus-%1$s-theme-scale-2 1.15
           modus-%1$s-theme-scale-3 1.21
           modus-%1$s-theme-scale-4 1.27
           modus-%1$s-theme-scale-5 1.33)
     (load-theme 'modus-%1$s t))
   theme))

(defun modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn
        (disable-theme 'modus-operandi)
        (modus-vivendi-theme-load))
    (disable-theme 'modus-vivendi)
    (modus-operandi-theme-load)))

(modus-operandi-theme-load)

(global-set-key (kbd "<s-clear>") 'modus-themes-toggle)
(global-set-key (kbd "s-9") 'modus-themes-toggle)

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
