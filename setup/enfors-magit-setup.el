;;; enfors-magit-setup --- Set up for Emacs git integration

;;; Commentary:
;;
;; To install magit, first run this file, then:
;;
;;   M-x install-package RET magit RET
;;
;;; Code:
;;
;; The following three lines makes it possible to install magit:
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

;; Set up a global key binding for magit-status
(global-set-key (kbd "C-x g") 'magit-status)

;; require magit so that it's loaded, so we can set the colors:
(require 'magit)
(set-face-foreground 'magit-hash                   "purple")

(set-face-foreground 'magit-diff-added             "purple")
(set-face-foreground 'magit-diff-added-highlight   "black")

(set-face-foreground 'magit-diff-base              "black")
(set-face-foreground 'magit-diff-base-highlight    "white")

(provide 'enfors-magit-setup)
;;; enfors-magit-setup ends here
