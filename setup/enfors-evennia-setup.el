;;;; enfors-evennia-setup --- Major mode for Evennia batch scripts

;;; Commentary:

;;; Code:

(setq evennia-highlights
      '(("@dig\\|@open\\|@tunnel\\|@desc\\|@tel\\|@create\\|@name" .
	 font-lock-keyword-face)
	("^Intent [A-Za-z0-9]+" .
	 font-lock-function-name-face)
	("^User:\\|^Bot:" .
	 font-lock-variable-name-face)
	("^#.*" .
	 font-lock-comment-face)))

(define-derived-mode evennia-mode text-mode "Evennia"
  "major mode for editing Evenna batch scripts."
  (setq font-lock-defaults '(evennia-highlights)))

(add-to-list 'auto-mode-alist '("\\.ev\\'" . evennia-mode))

(provide 'evennia-mode)
(provide 'enfors-evennia-setup)

;;; enfors-evennia-setup ends here
