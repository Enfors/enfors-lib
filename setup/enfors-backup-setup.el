;;;; enfors-backup-setup.el - Set up backup configuration
;;;; Based on http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files

;; Set directory to save backups to.
(setq backup-directory-alist '(("." . "~/.saves")))

(setq backup-by-copying t)

(setq delete-old-versions t
      kept-new-versions   6
      kept-old-versions   2
      version-control     t)

;; If you really want NO backup files, do the following (not recommended):
; (setq make-backup-files nil)

(provide 'enfors-backup-setup)
