;;;; enfors-emlb-setup --- Major mode for EnforsML bots

;;; Commentary:

;;; Code:

(setq emlb-highlights
      '(("@enable\\|@disable\\|@require\\|@storeinput\\|@goto" .
	 font-lock-keyword-face)
	("^Intent [A-Za-z0-9]+" .
	 font-lock-function-name-face)
	("^User:\\|^Bot:" .
	 font-lock-variable-name-face)
	("#.*" .
	 font-lock-comment-face)))

(define-derived-mode emlb-mode text-mode "EMLB"
  "major mode for editing EnforsML bots."
  (setq font-lock-defaults '(emlb-highlights)))

(add-to-list 'auto-mode-alist '("\\.emlb\\'" . emlb-mode))

(provide 'emlb-mode)
(provide 'enfors-emlb-setup)

;;; enfors-emlb-setup ends here
