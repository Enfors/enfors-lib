;;;; enfors-gnuplot-setup.el - Set up gnuplot-mode
;;;; Based on https://github.com/mkmcc/gnuplot-mode
;;;; Assumes ~/devel/elisp/gnuplog-mode is present.

;; make sure file is visible to emacs (if needed)
(add-to-list 'load-path "~/devel/elisp/gnuplot-mode")

;; load the file
(require 'gnuplot-mode)

;; specify the gnuplot executable (if other than /usr/bin/gnuplot)
;(setq gnuplot-program "/sw/bin/gnuplot")

;; automatically open files ending with .gp or .gnuplot in gnuplot mode
(setq auto-mode-alist 
      (append '(("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode)) auto-mode-alist))

(provide 'enfors-gnuplot-setup)
