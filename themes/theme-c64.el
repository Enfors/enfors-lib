
(set-foreground-color          "lightblue")
(set-background-color          "#203099")

;;;; Faces
(set-face-foreground 'default                      "lightblue")
(set-face-foreground 'font-lock-string-face        "yellow")
(set-face-foreground 'font-lock-keyword-face       "#ee44ee")
(set-face-foreground 'font-lock-comment-face       "darkcyan")
(set-face-foreground 'font-lock-variable-name-face "green")
(set-face-foreground 'font-lock-constant-face      "red")

;;;; Org-specific faces
(require 'enfors-org-setup)
;; org-hide needs to be the same as the background color
(set-face-foreground 'org-hide                     "#203099")
(set-face-foreground 'org-todo                     "red")
