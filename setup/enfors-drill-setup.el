;;; enfors-drill-setup.el --- Setup for org-drill
;;; Commentary:
;;; Code:
(use-package org-drill
  :ensure t
  :config
  (defun org-drill-time-to-inactive-org-timestamp (time)
    "Convert TIME into org-mode timestamp."
    (format-time-string (concat "[" (cdr org-time-stamp-formats) "]") time)))

(provide 'enfors-drill-setup)
;;; enfors-drill-setup.el ends here
