;;;; Org-mode stuff
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w@)" "VERIFY(v)" "|"
		  "DONE(d!)" "DELEGATED(e@)" "CANCELLED(c@)")))

;;; Always use org-indent-mode
(setq org-startup-indented t)

;;; Hide the all but the last star in headings
(setq org-hide-leading-stars t)

;;; Always use auto-fill-mode in org-mode
;(add-hook 'org-mode-hook 'turn-on-auto-fill)
;;; Actually, use visual-line-mode instead for soft wrap
(add-hook 'org-mode-hook 'visual-line-mode)

;;; Colors
(add-hook 'org-mode-hook '(lambda ()
                            (set-face-foreground 'org-target   "yellow")
                            (set-face-foreground 'org-checkbox "blue")
                            ))

;; Always display the empty line between headings
(setq org-cycle-separator-lines 1)

;; Define function for loading all org files in directory
(defun enfors-load-all-org-files-in-directory (directory)
  "Load all files ending with .org from specified directory."
  (interactive "sEnter directory: ")
  (dolist (file (directory-files directory t "\.org$"))
    (find-file file)))

;; Custom keys setup, from https://orgmode.org/manual/Activation.html:
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Show time reports as hours and minutes, never days
(setq org-duration-format (quote h:mm))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-clock-into-drawer t)
 '(org-log-into-drawer t))

(provide 'enfors-org-setup)
