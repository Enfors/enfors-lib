;;;; Package manager stuff

;; As suggested by the book Mastering Emacs:
(setq package-archives
      '(("gnu"       . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa"     . "http://melpa.milkbox.net/packages/")))

;;;; Common lisp
;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;(setq inferior-lisp-program "/home/enfors/ccl/armcl")

;;;; General emacs stuff
(setq transient-mark-mode t)
(setq sentence-end-double-space nil)	; For filling
(setq next-screen-context-lines   3)

;; This doesn't work for some reason (probably because terminal)
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

;; Key for opening journal file
(global-set-key (kbd "C-x j")       '(lambda ()
				       (interactive)
				       (find-file "~/plan/journal.org")))
;; IMenu - used to navigate. Press M-i tab for list of destinations.
(global-set-key (kbd "M-i") 'imenu)

;; Switch windows backwards with M-o
(global-set-key (kbd "M-o") (lambda ()
			      (interactive)
			      (other-window -1)))

(show-paren-mode 1)			; Always show matching parens.



;; Load enfors-lib
(or (boundp 'enfors-path)
    (setq enfors-path "/home/enfors/devel/elisp/enfors-lib"))
(load-file (concat enfors-path "/enfors-lib.el"))

(require 'ido)
(ido-mode t)
