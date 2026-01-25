;;;; Org-mode stuff
(require 'org)
(require 'org-habit)
(require 'org-capture)

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w@)" "VERIFY(v)" "|"
		  "DONE(d)" "DELEGATED(e@)" "CANCELLED(c@)")))

;;; Always use org-indent-mode
(setq org-startup-indented t)

;;; Hide the all but the last star in headings
(setq org-hide-leading-stars t)

;;; Always use auto-fill-mode in org-mode
;(add-hook 'org-mode-hook 'turn-on-auto-fill)
;;; Actually, use visual-line-mode instead for soft wrap
(add-hook 'org-mode-hook 'visual-line-mode)

;;; Colors
(add-hook 'org-mode-hook (lambda ()
                           (set-face-foreground 'org-target   "yellow")
                           (set-face-foreground 'org-checkbox "blue")
                           (bind-key "C-c d" 'enfors-dice)
                           ))

;; Always display the empty line between headings
(setq org-cycle-separator-lines 1)

;; Always force an empty line before every new heading/task
(setq org-blank-before-new-entry 
        '((heading . t)
          (plain-list-item . auto)))

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
;; Make Enter follow links instead of inserting an Enter into them.
(setq org-return-follows-link t)  ; Now use C-c C-l to edit links

;; The following two functions were created by Gemini 3 (don't shoot me).
;; The add s-Enter to indent to same level as leading "-" on previous line.
(defun my-org-dumb-newline ()
  "Insert newline and copy the previous line's indentation (block indent)."
  (interactive)
  (newline)
  (indent-relative))

(with-eval-after-load 'org
  ;; Bind Shift-Return to the 'dumb' newline
  (define-key org-mode-map (kbd "S-<return>") #'my-org-dumb-newline))
;; End of code by Gemini

;; Show time reports as hours and minutes, never days
(setq org-duration-format (quote h:mm))

;; Colors
(set-face-foreground 'org-table "#aa88cc")

;;; Agenda view
(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done  t
      org-agenda-remove-tags            t
      org-agenda-start-on-weekday       nil ;; Start today, not on Monday
      org-agenda-span                   7)  ;; Show next 7 days

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-clock-into-drawer t)
 '(org-log-into-drawer t))

;; Agenda
;(add-to-list 'org-agenda-files "~/devel/RoamNotes/20220513130808-hitachi.org")

(provide 'enfors-org-setup)
