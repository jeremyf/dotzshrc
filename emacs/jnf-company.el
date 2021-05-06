;;; jnf-company.el --- Summary
;;
;;; Commentary:
;;
;;  Extracting company usage
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :straight t
  :diminish
  :bind (("C-." . #'company-capf)
         (:map company-active-map (("<tab>" . 'company-select-next-if-tooltip-visible-or-complete-selection)
                                   ("C-<tab>" . 'company-select-previous))))
  :hook (after-init . global-company-mode)
  :init
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  (setq completion-cycle-threshold 3)

  :custom
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length 3)
  (company-tooltip-idle-delay 0.25 "Faster!")
  (company-async-timeout 20 "Some requests can take a long time. That's fine.")
  (company-selection-wrap-around t)
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

(use-package company-emoji
  :straight (company-emoji :type git :host github :repo "dunn/company-emoji" :branch "trunk")
  :config (add-to-list 'company-backends 'company-emoji))

(provide 'jnf-company.el)
;;; jnf-company.el ends here
