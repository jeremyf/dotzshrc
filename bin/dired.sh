#!/bin/bash

# Launch Dired in a plain Emacs configuration.

# Arguments are passed to Emacs, e.g. "-nw" works as expected.

emacs -q "$@" \
      --eval "(dired default-directory)" \
      --eval "(defun kill-window-or-emacs () (interactive) (if (one-window-p) (kill-emacs) (delete-window)))" \
      --eval "(setq dired-dwim-target t delete-by-moving-to-trash t)" \
      --eval "(define-key dired-mode-map (kbd \"q\") #'kill-window-or-emacs)"
