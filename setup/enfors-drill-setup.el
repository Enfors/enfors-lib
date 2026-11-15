;;; enfors-drill-setup.el --- Setup for org-drill
;;; Commentary:
;;; Code:
(use-package org-drill
  :ensure t
  :config
  ;; Prevent the "pile of doom" by capping how much it asks of you per session.
  ;; Even if 100 cards are overdue, it will only serve you 30 at a time.
  (setq org-drill-maximum-items-per-session 30)
  (defun org-drill-time-to-inactive-org-timestamp (time)
    "Convert TIME into org-mode timestamp."
    (format-time-string (concat "[" (cdr org-time-stamp-formats) "]") time)))

(provide 'enfors-drill-setup)
;;; enfors-drill-setup.el ends here
