;;;; tools.el by Christer Enfors

(defun enfors-date-insert ()
  "Insert today's date in the current buffer."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

    
