;;; jnf-display.el --- Summary -*- lexical-binding: t; -*-
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
(add-to-list 'default-frame-alist '(font . "JetBrains Mono 14" ))
(set-face-attribute 'default t :font "JetBrains Mono 14" )
;; I'm just going to trust themes
(setq custom-safe-themes t)

(require 'jnf-modus-main.el)


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
(set-face-attribute 'variable-pitch nil :family "ETBembo" :height 1.0)
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

;; Disabled because on 2021-04-11 I got the following error:
;; *ERROR*: Symbol’s value as variable is void: file
;;
;; Incorporates file icons with file listings of dired
;; (use-package all-the-icons-dired
;;   :straight t
;;   :after all-the-icons
;;   :hook (dired-mode . all-the-icons-dired-mode))

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
;; (use-package smartparens
;;   :straight t
;;   :config (smartparens-strict-mode 1)
;;   (smartparens-global-mode 1))

;; A convenience function to create a nice string
(defun with-faicon (icon str &optional height v-adjust)
    "Displays an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-material (icon str &optional height v-adjust)
    "Displays an icon from Font Awesome icon."
    (s-concat (all-the-icons-material icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(use-package olivetti
  :straight (olivetti :type git :host github :repo "rnkn/olivetti"))

(defun jnf/toggle-writing-mode ()
  "A simple toggle for a few nice writing mode behaviors."
  (interactive)
  (progn  (olivetti-mode 'toggle) (variable-pitch-mode 'toggle)))

(provide 'jnf-display.el)
;;; jnf-display.el ends here
