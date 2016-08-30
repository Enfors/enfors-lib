(defun enf-set-theme (enf-theme)
  "Set the color scheme."
  (interactive "sEnter theme name: ")
  (load-file (concat enf-path "/themes/theme-" enf-theme ".el")))
