;;; jnf-basic-config.el --- Summary
;;
;;; Commentary:
;;
;;  Some basic configurations that should be applicable for all emacs
;;  settings.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-frame-font "MesloLGS NF")

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
;; See https://snarfed.org/gnu_emacs_backup_files
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; Stop ringing any bell
(setq ring-bell-function 'ignore)

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
(provide 'jnf-basic-config.el)
;;; jnf-basic-config.el ends here
