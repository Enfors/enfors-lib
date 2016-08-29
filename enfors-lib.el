;;;; enfors-lib/enfors-lib.el - main file.

(setq enf-dir   "/home/enfors/devel/elisp/enfors-lib")
(add-to-list    'load-path enf-dir)

(setq enf-files '("toys.el"))

(mapcar (lambda (enf-file)
	  (load-file (concat enf-dir "/" enf-file)))
	enf-files)


