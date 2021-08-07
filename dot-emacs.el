;;;; Package manager stuff

;; As suggested by the book Mastering Emacs:
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
;; To install packages: M-x package-install RET
;; If you have added an archive, you might need to
;; (package-refresh-contents) first.

;;;; Common lisp
;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;(setq inferior-lisp-program "/home/enfors/ccl/armcl")

;;;; General emacs stuff
(setq transient-mark-mode t)
(setq sentence-end-double-space nil)	; For filling
(setq next-screen-context-lines   3)
(setq-default indent-tabs-mode  nil)    ; Use spaces, not tabs
(menu-bar-mode                    -1)   ; Get rid of the filthy menu bar
(tool-bar-mode                    -1)   ; and its ugly tool bar cousin

;; This doesn't work for some reason (probably because terminal)
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

;; Key for opening journal file
;(global-set-key (kbd "C-x j")       '(lambda ()
;				       (interactive)
;				       (find-file "~/plan/journal.org")))
;; IMenu - used to navigate. Press M-i tab for list of destinations.
(global-set-key (kbd "M-i") 'imenu)

;; Switch windows backwards with M-o
(global-set-key (kbd "M-o") (lambda ()
			      (interactive)
			      (other-window -1)))

(show-paren-mode 1)			; Always show matching parens.

;; Horizontal scroll
; 'current-line scrolls only current line. nil disables scrolling.
(setq auto-hscroll-mode 'current-line)

(add-to-list 'auto-mode-alist '("\\.lr\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'visual-line-mode)

;; Weeks start on Mondays. Americans are crazy.
(setq calendar-week-start-day 1)  ; 0 = Sunday, 1 = Monday, etc.

;; Load enfors-lib
(or (boundp 'enfors-path)
    (setq enfors-path "~/devel/elisp/enfors-lib"))
(load-file (concat enfors-path "/enfors-lib.el"))

(require 'ido)
(ido-mode t)
