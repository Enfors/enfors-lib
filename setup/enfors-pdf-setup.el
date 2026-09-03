;;; enfors-pdf-setup --- My PDF configuration
;;; Commentary:
;;; Code:

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; Initialize the package; 't' prevents it from propting you
  ;; for confirming if the epdfinfo server ever needs to recompile
  (pdf-tools-install t)

  ;; Optional: Sets the default zoom level to fit the page on screen
  (setq-default pdf-view-display-size 'fit-page)

  :hook
  ;; Enable live preview updates
  (pdf-view-mode . auto-revert-mode)

  ;; Uncomment the line below if you use a dark theme
  ;; and want PDFs to automatically invert their colors:
  ;; (pdf-view-mode . pdf-view-midnight-minor-mode)
  )

(provide 'enfors-pdf-setup)
;;; enfors-pdf-setup.el ends here
