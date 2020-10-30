;;; jnf-swiper.el --- Summary
;;
;;; Commentary:
;;
;;  Here I configure the Swiper function.  Early in Emacs, I mapped
;;  "C-s" to swiper.  It works alright.  However, I'm also exploring
;;  using helm-swoop instead.  The swoop provides an easier edit
;;  matches functionality.
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I use this all of the freaking time to peak at other parts of my
;; code.  It's also a navigation tool.
;; (use-package swiper
;;   :straight t
;;   :bind (("C-s" . swiper)))

;; https://github.com/emacsorphanage/helm-swoop
;;
;; An interesting option compared to Swiper.  The prompts provide
;; clear guidance on how to edit these results.  I may have convinced
;; myself to switch from Swiper.
(use-package helm-swoop
  :straight t
  :bind (("C-s" . helm-swoop)
         (:map helm-swoop-map
               ;; C-w is typically mapped in helm-swoop mode.
               (("C-w" . jnf/kill-region-or-backward-word))
               )))
(provide 'jnf-swiper.el)
;;; jnf-swiper.el ends here
