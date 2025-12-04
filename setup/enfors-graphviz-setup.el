;;; enfors-graphviz-setup.el
;;
;; Requirement:
;; # apt install graphviz
;;

(require 'ob)  ;; Load babel

(custom-set-variables
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (dot . t))))

;(org-babel-do-load-langauges
; 'org-babel-load-languages
; '((dot . t))) ;; Enable DOT

(provide 'enfors-graphviz-setup)
