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
(set-face-foreground 'company-tooltip              "blue")
(set-face-foreground 'helm-selection               "white")
(set-face-foreground 'org-level-1                  "#8080dd")
(set-face-foreground 'org-drawer                   "#88aaaa")
(set-face-foreground 'org-document-info-keyword    "#777777")
(set-face-foreground 'org-hide                     "#140624")
(set-face-foreground 'org-table                    "#6666cc")

(set-face-foreground 'custom-comment-tag           "#88aaaa")

(set-face-foreground 'dired-directory              "#6666cc")

(set-face-foreground 'epa-string                   "#6666cc")

(set-face-foreground 'minibuffer-prompt            "#9966cc")
;(set-face-foreground 'helm-minibuffer-prompt       "#9966cc")

;;;; Set in other files:

;; enfors-roam-setup: org-document-title
