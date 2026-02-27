;;;; enfors-elisp-setup.el -- My setup for editing elisp files

;;; Commentary:

;;; Code:

(require 'flymake)
(show-paren-mode 1)
(add-hook 'emacs-lisp-mode-hook #'electric-pair-local-mode)
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)
(add-hook 'emacs-lisp-mode-hook #'column-number-mode)

;; To show errors when eldoc overwrites them in minibuffer: C-h .
;; To show diagnostics buffer: M-x flymake-show-buffer-diagnostics
(setq eldoc-echo-area-use-multiline-p t)

(define-key emacs-lisp-mode-map (kbd "M-n") #'flymake-goto-next-error)
(define-key emacs-lisp-mode-map (kbd "M-p") #'flymake-goto-prev-error)

(use-package company
  :ensure t
  :config
  :hook (emacs-lisp-mode . company-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (emacs-lisp-mode . rainbow-delimiters-mode)
  :config
  ;; Loop through all 9 depth levels and force them to be bold
  (dolist (face '(rainbow-delimiters-depth-1-face
                  rainbow-delimiters-depth-2-face
                  rainbow-delimiters-depth-3-face
                  rainbow-delimiters-depth-4-face
                  rainbow-delimiters-depth-5-face
                  rainbow-delimiters-depth-6-face
                  rainbow-delimiters-depth-7-face
                  rainbow-delimiters-depth-8-face
                  rainbow-delimiters-depth-9-face))
    (set-face-attribute face nil :weight 'bold)))

(defun enfors-outshine-cycle-and-reset ()
  "Cycle buffer visibility and ensure point is at the start of the heading."
  (interactive)
  (outshine-cycle-buffer)
  ;; Pull the cursor out of the hidden text and onto the active headers
  (ignore-errors (outline-back-to-heading t))
  (beginning-of-line))

(use-package outshine
  :ensure t
  :hook (emacs-lisp-mode . outshine-mode)
  :bind (:map outshine-mode-map
              ("<backtab>" . enfors-outshine-cycle-and-reset)
              ("S-<tab>"   . enfors-outshine-cycle-and-reset))
  :config
  ;; Choose between `show-all' or `show-all' here:
  (setq outshine-startup-folded-p 'hide-body))

(provide 'enfors-elisp-setup)

;;; enfors-elisp-setup.el ends here
