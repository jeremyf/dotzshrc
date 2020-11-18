;;; jnf-ruby.el --- Summary -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  This package provides most of the Ruby configuration
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I most often write tests using rspec.
(use-package rspec-mode
  :straight t
  :after inf-ruby
  :bind (:map rspec-mode-map (("s-." . 'rspec-toggle-spec-and-target)))
  :hook (ruby-mode . rspec-mode)
  (ruby-mode . eldoc-mode))

(defun rspec-hyrax ()
  "Setup rspec mode docker configuration for Hyrax."
  (interactive)
  (setq rspec-docker-command "docker-compose exec -w /app/samvera/hyrax-engine")
  (setq rspec-docker-cwd "/app/samvera/hyrax-engine/")
  (setq rspec-docker-container "app"))

;; I most often write documentation using yard.  See
;; https://yardoc.org.
(use-package yard-mode
  :straight t
  :hook (ruby-mode . yard-mode))

(provide 'jnf-ruby.el)
;;; jnf-ruby.el ends here
