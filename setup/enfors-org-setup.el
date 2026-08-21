;;; enfors-org-setup --- My org-mode configuration
;;; Commentary:
;;; Code:

;;; ----------------------------------------------------------------------------
;;; CORE ORG-MODE
;;; ----------------------------------------------------------------------------
(use-package org
  :ensure nil ; Built into Emacs
  :demand t
  :hook ((org-mode . visual-line-mode)
         (org-mode . (lambda ()
                       (set-face-foreground 'org-target "yellow")
                       (set-face-foreground 'org-checkbox "blue"))))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("S-<return>" . my-org-dumb-newline))
  :config
  ;; Custom functions
  (defun my-org-dumb-newline ()
    "Insert newline and copy the previous line's indentation (block indent)."
    (interactive)
    (newline)
    (indent-relative))
  
  (defun enfors-load-all-org-files-in-directory (directory)
    "Load all files ending with .org from specified DIRECTORY."
    (interactive "sEnter directory: ")
    (dolist (file (directory-files directory t "\.org$"))
      (find-file file)))

  ;; General settings
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "WAITING(w@)" "|"
                    "DONE(d)" "CANCELLED(c@)")))
  (setq org-startup-indented t
        org-log-done nil
        org-hide-leading-stars t
        org-cycle-separator-lines 1
        org-blank-before-new-entry '((heading . t) (plain-list-item . auto))
        org-duration-format (quote h:mm)
        org-return-follows-link t
        org-clock-into-drawer t
        org-log-into-drawer t)

  ;; Effort setup
  (setq org-global-properties
        '(("Effort_ALL" . "0:15 0:30 1:00 2:00 4:00")))
  (setq org-columns-default-format
        "%50ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM(Clocked)")
  ;; Sadly, org-agenda-columns-add-appointments-to-effort-sum seems to not work.
  ;; Therefore, I have the enfors-auto-effort-from-timestamp function further
  ;; down to fix that.
  (setq org-agenda-columns-add-appointments-to-effort-sum t)

  ;; Links
  (setf (alist-get 'file org-link-frame-setup) #'find-file))

(with-eval-after-load 'simple ; simple.el is where visual-line-mode lives
  (diminish 'visual-line-mode))
;;; ----------------------------------------------------------------------------
;;; ORG INDENT
;;; ----------------------------------------------------------------------------
(with-eval-after-load 'org-indent
  (diminish 'org-indent-mode))
;;; ----------------------------------------------------------------------------
;;; ORG AGENDA
;;; ----------------------------------------------------------------------------
(use-package org-agenda
  :ensure nil
  :after org
  :bind (:map org-agenda-mode-map
              ("M-<up>" . org-agenda-priority-up)
              ("M-<down>" . org-agenda-priority-down)
              ("RET" . org-agenda-switch-to))
  :config
  (setq org-agenda-skip-scheduled-if-done nil
        org-agenda-skip-deadline-if-done  t
        org-agenda-time-leading-zero      t
        org-agenda-skip-unavailable-files t
        org-deadline-warning-days         0
        org-agenda-remove-tags            nil
        org-agenda-tags-column            'auto
        org-habit-graph-column            45
        org-agenda-start-on-weekday       nil
        org-agenda-show-outline-path      t
        org-enforce-todo-dependencies     t
        org-agenda-dim-blocked-tasks      t
        org-agenda-echo-preserve-layout   t
        org-agenda-log-mode-items         '(closed clock state))

  (setq org-agenda-sorting-strategy
        '((agenda time-up priority-down todo-state-down effort-up scheduled-up)
          (todo   priority-down scheduled-up)
          (tags   priority-down scheduled-up)
          (search priority-down)))

  (setq org-agenda-time-grid
        '((daily today require-timed remove-match)
          (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000)
          "......"
          "----------------")))
;;; ----------------------------------------------------------------------------
;;; AUTO UPDATE EFFORT BASED ON DURATIONS IN CALENDAR FILE
;;; ----------------------------------------------------------------------------
;; I want my agenda column view to show how much time is planned - put into
;; "effort" - in total for each day. I add meetings to my calendar file with a
;; duration, but "duration" doesn't show up in the "effort" totals in agenda
;; column view. So these functions make sure that an effort is set automatically
;; on each entry in the calendar file when it is saved, based on each entry's
;; duration. So basically, it duplicates the data (from duration to effort)
;; automatically, so I don't have to do it manually. The functions were written
;; by Gemini Pro.
;; 
;; This is becaus org-agenda-columns-add-appointments-to-effort-sum doesn't seem
;; to work.
(defun enfors-auto-effort-from-timestamp ()
  "Find time ranges in Org timestamps and copy the duration to the Effort property."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^>]*? \\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)-\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)>" nil t)
      (let* ((start-time (match-string-no-properties 1))
             (end-time   (match-string-no-properties 2))
             (start-mins (org-duration-to-minutes start-time))
             (end-mins   (org-duration-to-minutes end-time))
             ;; Force the float into a clean integer here:
             (duration   (round (- end-mins start-mins))))
        (when (> duration 0)
          (let ((effort-string (format "%d:%02d" (/ duration 60)
                                       (% duration 60))))
            (save-excursion
              (org-back-to-heading t)
              (unless (org-entry-get nil "Effort")
                (org-set-property "Effort" effort-string)))))))))

(defun enfors-calendar-effort-hook ()
  "Trigger effort calculation only when saving the specific calendar file."
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name)
             (string-match-p "20260131184817-calendar\\.org$"
                             (buffer-file-name)))
    (enfors-auto-effort-from-timestamp)))

(add-hook 'before-save-hook #'enfors-calendar-effort-hook)

;;; ----------------------------------------------------------------------------
;;; ORG NOTIFICATIONS
;;; ----------------------------------------------------------------------------
(setq org-show-notification-handler
      (lambda (msg)
        (message "%s" msg)))
;;; ----------------------------------------------------------------------------
;;; ORG HABIT
;;; ----------------------------------------------------------------------------
(use-package org-habit
  :ensure nil
  :after org)


;;; ----------------------------------------------------------------------------
;;; ORG ROAM (Extensions & Hacks)
;;; ----------------------------------------------------------------------------
(use-package org-roam
  :after org
  :config
  ;; Modeline shortening
  (defun my/org-roam-rename-buffer-to-title ()
    "Rename the current buffer to the value of the #+title: keyword."
    (when (and (derived-mode-p 'org-mode)
               (org-roam-file-p))
      (let ((title (or (cadar (org-collect-keywords '("TITLE")))
                       (file-name-nondirectory (buffer-file-name)))))
        (when title
          (rename-buffer title t)))))
  
  (add-hook 'org-roam-find-file-hook #'my/org-roam-rename-buffer-to-title)
  
  ;; Backlinks sorting hack
  (defun enfors-org-roam-backlinks-section (node)
    "The 'Backlinks' section for Org-roam, sorted by file modification time."
    (when-let ((backlinks (org-roam-backlinks-get node)))
      (magit-insert-section (org-roam-backlinks)
        (magit-insert-heading "Backlinks")
        (dolist (backlink (sort backlinks
                                (lambda (a b)
                                  (time-less-p
                                   (org-roam-node-file-mtime
                                    (org-roam-backlink-source-node b))
                                   (org-roam-node-file-mtime
                                    (org-roam-backlink-source-node a))))))
          (org-roam-node-insert-section
           :source-node (org-roam-backlink-source-node backlink)
           :point (org-roam-backlink-point backlink)
           :properties (org-roam-backlink-properties backlink))))
      (insert "\n")))

  (setq org-roam-mode-sections
        (list #'enfors-org-roam-backlinks-section
              #'org-roam-reflinks-section)))


;;; ----------------------------------------------------------------------------
;;; ORG CRYPT
;;; ----------------------------------------------------------------------------
(use-package org-crypt
  :ensure nil
  :after org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-crypt-key "christer.enfors@gmail.com")
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-disable-auto-save t))

(use-package epa
  :ensure nil
  :custom
  (epa-pinentry-mode 'loopback))


;;; ----------------------------------------------------------------------------
;;; ORG EXPORT & BABEL
;;; ----------------------------------------------------------------------------
(use-package ox-latex
  :ensure nil
  :after org
  :config
  (setq org-latex-remove-logfiles t)
  (add-to-list 'org-latex-logfiles-extensions "tex"))

(use-package ob-core
  :ensure nil
  :after org
  :config
  (setq org-babel-python-command "python3")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t))))

;;; ----------------------------------------------------------------------------
;;; ORG TIME TABLES
;;; ----------------------------------------------------------------------------
(defvar enfors-clocktable-work-files
  '("20260131140138-tingvalla_moten.org"
    "20260319130840-utbildningsgruppen_moten.org"
    "20260402081109-unionen_moten.org"
    "20260813125653-cybersec_forum.org"
    "20230512084137-saab.org")
  "A list of work files to include in work time reports.")

(defvar enfors-clocktable-extra-files '()
  "A list of extra Org files to include in clocktables, along agenda files.")

(setq enfors-clocktable-extra-files enfors-clocktable-work-files)

(defun enfors-get-clocktable-files ()
  "Return a list of all agenda files plus `enfors-clocktable-extra-files`."
  (append (org-agenda-files) enfors-clocktable-extra-files))
(provide 'enfors-org-setup)
;;; enfors-org-setup.el ends here
