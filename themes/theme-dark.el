;;;; To find faces to customize: M-x list-faces-display RET

(set-background-color "#001138")
(set-foreground-color "#ccccff")
(set-cursor-color "green")

(require 'magit)
(require 'company)
(require 'helm)

;;;; Faces
(set-face-foreground 'default                      "white")
(set-face-foreground 'font-lock-string-face        "#cccc33")
(set-face-foreground 'font-lock-keyword-face       "magenta")
(set-face-foreground 'font-lock-comment-face       "cyan")
(set-face-foreground 'font-lock-variable-name-face "green")
(set-face-foreground 'font-lock-constant-face      "green")
(set-face-foreground 'magit-section-highlight      "#EE88CC")
(set-face-foreground 'magit-diff-removed-highlight "white")
(set-face-foreground 'magit-diff-added-highlight   "blue")
(set-face-foreground 'company-tooltip              "yellow")
(set-face-foreground 'helm-selection               "red")
