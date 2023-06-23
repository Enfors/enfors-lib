;;;; enfors-lib/enfors-lib.el - main file.

(or (boundp 'enfors-path)
    (setq   enfors-path   "~/devel/elisp/enfors-lib"))
(add-to-list 'load-path enfors-path)
(add-to-list 'load-path (concat enfors-path "/setup"))

(setq enfors-files '("toys.el" "themes.el" "tools.el" "urpg.el"))

(mapcar (lambda (enfors-file)
	  (load-file (concat enfors-path "/" enfors-file)))
	enfors-files)
