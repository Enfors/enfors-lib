;;; My own keyboard stuff
(defun enfors-ins-tilde ()
  "Insert ~ at point."
  (interactive)
  (insert "~"))

(defun enfors-ins-caret ()
  "Insert ^ at point."
  (interactive)
  (insert "^"))

(provide 'enfors-keyboard-setup)
