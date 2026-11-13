;;; tools.el --- some simple Elisp tools
;;; Commentary:
;;; Code:
(defun enfors-date-insert ()
  "Insert today's date in the current buffer."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun enfors-compound-interest (start interest-percent years)
  "Return final amount starting with START at INTEREST-PERCENT after YEARS."
  (let ((interest-factor (+ 1 (/ float interest-percent) 100)))
    (* start (expt interest-factor years))))

;;; tools.el ends here
