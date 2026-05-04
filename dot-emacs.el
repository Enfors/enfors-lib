;;; dot-emacs.el --- my ".emacs" for enfors-lib
;;; Commentary:
;;; Code:

;; Force English timestamps (Mon, Tue, Wed) to prevent Org Mode corruption
(setq system-time-locale "C")

;; Weeks start on Mondays. Americans are crazy.
(setq calendar-week-start-day 1)  ; 0 = Sunday, 1 = Monday, etc.

;;; Package manager stuff

;; As suggested by the book Mastering Emacs:
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa.org/packages/") t)
(package-initialize)

;;; General emacs stuff

(setq transient-mark-mode t)
(setq sentence-end-double-space nil)	; For filling
(setq next-screen-context-lines   3)
(setq-default indent-tabs-mode  nil)    ; Use spaces, not tabs
(menu-bar-mode                    -1)   ; Get rid of the filthy menu bar
(unless (ignore-errors (tool-bar-mode -1))   ; and its ugly tool bar cousin
  (message "Unable to turn off toolbar mode."))
;; This doesn't work for some reason (probably because terminal)
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

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

;;; Windmove setup

(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings))       ; S-arrows to switch windows

;;; Prevent Emacs from opening stuff in the wrong window

;; Force Emacs to open buffers in the currently focused window
(setq display-buffer-base-action
      '((display-buffer-same-window)
        (inhibit-same-window . nil)))

;; Prevent Emacs from aggressively splitting windows on its own
(setq pop-up-windows nil)

;;; Load enfors-lib
(or (boundp 'enfors-path)
    (setq enfors-path "~/devel/elisp/enfors-lib"))
(load-file (concat enfors-path "/enfors-lib.el"))

;(require 'ido)
;(ido-mode t)

;;; Helm setup

(setq helm-buffer-details-flag nil)
(setq helm-buffer-max-length 40)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b")   'helm-mini)

(provide 'dot-emacs)

;;; dot-emacs.el ends here
