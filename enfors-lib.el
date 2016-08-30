;;;; enfors-lib/enfors-lib.el - main file.

(or (boundp 'enf-path)
    (setq   enf-path   "/home/enfors/devel/elisp/enfors-lib"))
(add-to-list 'load-path enf-path)

(setq enf-files '("toys.el" "themes.el"))

(mapcar (lambda (enf-file)
	  (load-file (concat enf-path "/" enf-file)))
	enf-files)


