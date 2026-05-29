;;; theme-dark.el -- Theme
;;; Commentary:
;;; Code:

;; To find faces to customize: M-x list-faces-display RET

(set-background-color "#001138")
;; Set the background color of the Org "C-c C-c Select tag preview"
(set-face-attribute 'secondary-selection nil :background "#1E3A8A")
(set-foreground-color "#ccccff")
(set-cursor-color "green")

(require 'magit)
(require 'company)
(require 'helm)

;; Faces
(set-face-foreground 'default                      "#9090d0")
;(set-face-foreground 'bold                         "#6040a0")
; (set-face-attribute  'bold nil :foreground "#6040a0" :weight 'bold) ; Darker
(set-face-attribute  'bold nil :foreground "#c0b8f0" :weight 'bold) ; Brighter
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
(set-face-foreground 'helm-match                   "#6060ff")
(set-face-foreground 'org-level-1                  "#6060ff")
(set-face-foreground 'org-level-2                  "#44ffa0")
(set-face-foreground 'org-level-4                  "#44aaff")
(set-face-foreground 'org-todo                     "#cc4488")
(set-face-foreground 'org-done                     "#44cc88")
(set-face-foreground 'org-link                     "#44ccdd")
(set-face-foreground 'org-drawer                   "#88aaaa")
(set-face-foreground 'org-document-info-keyword    "#777777")
(set-face-foreground 'org-hide                     "#140624")
(set-face-foreground 'org-table                    "#8866cc")
(set-face-foreground 'org-verbatim                 "#c0b070")

(set-face-foreground 'dired-directory              "#6666cc")

(set-face-foreground 'epa-string                   "#6666cc")

(set-face-foreground 'minibuffer-prompt            "#9966cc")
(set-face-foreground 'helm-minibuffer-prompt       "#9966cc")

(set-face-attribute  'company-tooltip nil
                     :background "white smoke" :foreground "blue")
(set-face-attribute  'company-tooltip-common nil
                     :foreground "dark slate blue")
(set-face-attribute  'company-tooltip-selection nil
                     :background "gray")
(set-face-foreground 'fill-column-indicator        "#4a4466")
 ;; '(company-tooltip ((t (:background "white smoke" :foreground "blue"))))
 ;; '(company-tooltip-common ((t (:foreground "dark slate blue"))))
 ;; '(company-tooltip-selection ((t (:background "gray")))))
;;; Set in other files:

;; enfors-roam-setup: org-document-title

(provide 'theme-dark)
;;; theme-dark.el ends here
