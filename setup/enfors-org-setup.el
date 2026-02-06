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
      org-deadline-warning-days         0   ;; How many days in advance to warn
      org-agenda-remove-tags            nil
      org-agenda-tags-column            80
      org-agenda-start-on-weekday       nil ;; Start today, not on Monday
      org-agenda-show-outline-path      t   ;; Show outline in message buffer
      org-enforce-todo-dependencies     t   ;; All children must be DONE before parent
      org-agenda-dim-blocked-tasks      t
      org-agenda-echo-preserve-layout   t   ;; Keep breadcrumb visible in msg area
      org-deadline-warning-days         0   ;; Don't show future deadlines today
      )

(setq org-agenda-time-grid
      '((daily today require-timed remove-match)
        (800 900 1000 1100 1200 1300 1400 1500 1600 1700)
        "......"
        "----------------"))

;;; Hack to get backlinks to sort in order of file modification date,
;;; rather than alphabetical sort of file name.
;;; Future versions of org-roam has org-roam-backlinks-sort - once I
;;; get that version, this hack will be obsolete.
(defun enfors-org-roam-backlinks-section (node)
  "The 'Backlinks' section for Org-roam, sorted by file modification time."
  (when-let ((backlinks (org-roam-backlinks-get node)))
    (magit-insert-section (org-roam-backlinks)
      (magit-insert-heading "Backlinks")
      (dolist (backlink (sort backlinks (lambda (a b)
                                          (time-less-p 
                                           (org-roam-node-file-mtime (org-roam-backlink-source-node b))
                                           (org-roam-node-file-mtime (org-roam-backlink-source-node a))))))
        (org-roam-node-insert-section
         :source-node (org-roam-backlink-source-node backlink)
         :point (org-roam-backlink-point backlink)
         :properties (org-roam-backlink-properties backlink))))
    (insert "\n")))

;; Tell Org-roam to use OUR function instead of the default
(setq org-roam-mode-sections
      (list #'enfors-org-roam-backlinks-section
            #'org-roam-reflinks-section))
;;; End of backlinks sorting hack.

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
