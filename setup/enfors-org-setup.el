;;;; Org-mode stuff
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@)" "VERIFY(v)" "|"
		  "DONE(d!)" "DELEGATED(e@)" "CANCELLED(c@)")))

;;; Always use org-indent-mode
(setq org-startup-indented t)

;;; Hide the all but the last star in headings
(setq org-hide-leading-stars t)

;;; Always use auto-fill-mode in org-mode
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook '(lambda () (set-face-foreground 'org-target "yellow")))

;; Always display the empty line between headings
(setq org-cycle-separator-lines 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-clock-into-drawer t)
 '(org-log-into-drawer t))

(provide 'enfors-org-setup)
