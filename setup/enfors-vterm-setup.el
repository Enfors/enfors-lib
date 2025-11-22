;;;; vterm
;;; Code:
(use-package vterm
  :ensure t
  :config
  ;; Optional: Set the shell explicitly to bash (defaults to system shell)
  (setq vterm-shell "/bin/bash"))

(provide 'enfors-vterm-setup)
