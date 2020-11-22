;;; jnf-modus-master.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This package provides the "master" branch method for loading modus themes.
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Based on system type, either load the OSX apperance (e.g. dark or
;; light) and load accordingly.
(if (eq system-type 'darwin)
    (progn
      (defun jnf-dark ()
        "Toggle system-wide Dark or Light setting."
        (interactive)
        (shell-command "osascript -e 'tell application \"System Events\" to tell appearance preferences to set dark mode to not dark mode'")
        (jnf-emacs-theme-by-osx-appearance))

      (defalias 'modus-themes-toggle 'jnf-dark)
      (defun jnf-emacs-theme-by-osx-appearance ()
        "Set theme based on OSX apperance state."
        (if (equal "Dark" (substring (shell-command-to-string "defaults read -g AppleInterfaceStyle") 0 4))
            (progn
              (disable-theme 'modus-operandi)
              (modus-vivendi-theme-load))
          (progn
            (disable-theme 'modus-vivendi)
            (modus-operandi-theme-load))
          ))
      (jnf-emacs-theme-by-osx-appearance))
  (progn
    (defun modus-themes-toggle ()
      "Toggle between `modus-operandi' and `modus-vivendi' themes."
      (interactive)
      (if (eq (car custom-enabled-themes) 'modus-operandi)
          (progn
            (disable-theme 'modus-operandi)
            (modus-vivendi-theme-load))
        (disable-theme 'modus-vivendi)
        (modus-operandi-theme-load)))
    (modus-operandi-theme-load)))

(global-set-key (kbd "<s-clear>") 'modus-themes-toggle)
(global-set-key (kbd "s-9") 'modus-themes-toggle)
(provide 'jnf-modus-master.el)
;;; jnf-modus-master.el ends here
