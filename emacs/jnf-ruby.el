;;; jnf-ruby.el --- Summary
;;
;;; Commentary:
;;
;;  This package provides most of the Ruby configuration
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package robe
  :after company
  :straight t
  :ensure t
  :custom (ruby-insert-encoding-magic-comment nil)
  :mode (("\\.erb\\'" . web-mode))
  :hook ((ruby-mode . robe-mode)
         (ruby-mode . ac-robe-setup)))

;; I most often write tests using rspec.
(use-package rspec-mode
  :straight t
  :after inf-ruby
  :ensure t
  :bind (:map rspec-mode-map (("s-." . 'rspec-toggle-spec-and-target)))
  :hook (ruby-mode . rspec-mode))

;; I most often write documentation using yard.  See https://yardoc.org.
(use-package yard-mode
  :straight t
  :ensure t
  :hook (ruby-mode . yard-mode))


;; A package to run a ruby process inside emacs.
(use-package inf-ruby
  :straight t
  :after company
  :ensure t)
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
(provide 'jnf-ruby.el)
;;; jnf-ruby.el ends here
