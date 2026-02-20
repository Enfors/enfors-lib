;;;; enfors-elisp-setup.el -- My setup for editing elisp files

;;; Commentary:

;;; Code:

(require 'flymake)
(show-paren-mode 1)
(add-hook 'emacs-lisp-mode-hook #'electric-pair-local-mode)
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)
;; To show errors when eldoc overwrites them in minibuffer: C-h .
(setq eldoc-echo-area-use-multiline-p t)

(define-key emacs-lisp-mode-map (kbd "M-n") #'flymake-goto-next-error)
(define-key emacs-lisp-mode-map (kbd "M-p") #'flymake-goto-prev-error)

(provide 'enfors-elisp-setup)

;;; enfors-elisp-setup.el ends here
