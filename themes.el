(defun enfors-set-theme (enfors-theme)
  "Set the color scheme."
  (interactive "sEnter theme name: ")
  (load-file (concat enfors-path "/themes/theme-" enfors-theme ".el")))
