;;;; enfors-lib/enfors-lib.el - main file.

;;; Code:

(or (boundp 'enfors-path)
    (setq   enfors-path   "~/devel/elisp/enfors-lib"))
(add-to-list 'load-path enfors-path)
(add-to-list 'load-path (concat enfors-path "/setup"))

;; This mode is for clearing up the statusbar
(use-package diminish
  :ensure t)

(add-to-list 'load-path (concat enfors-path "/ttrpg"))

(setq enfors-files '("toys.el" "themes.el" "tools.el" "urpg.el" "dice.el"))

(mapcar (lambda (enfors-file)
	  (load-file (concat enfors-path "/" enfors-file)))
	enfors-files)

(require 'enfors-ttrpg-dashboard)
