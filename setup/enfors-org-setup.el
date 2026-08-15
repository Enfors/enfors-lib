;;; enfors-org-setup --- My org-mode configuration
;;; Commentary:
;;; Code:

;;; ----------------------------------------------------------------------------
;;; CORE ORG-MODE
;;; ----------------------------------------------------------------------------
(use-package org
  :ensure nil ; Built into Emacs
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
        '((sequence "TODO(t)" "STARTED(s)" "WAITING(w@)" "VERIFY(v)" "|"
                    "DONE(d)" "DELEGATED(e@)" "CANCELLED(c@)")))
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

  ;; Links
  (setf (alist-get 'file org-link-frame-setup) #'find-file))


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
  (setq org-agenda-skip-scheduled-if-done t
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
        '((agenda time-up priority-down scheduled-up)
          (todo   priority-down scheduled-up)
          (tags   priority-down scheduled-up)
          (search priority-down)))

  (setq org-agenda-time-grid
        '((daily today require-timed remove-match)
          (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000)
          "......"
          "----------------")))


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

(provide 'enfors-org-setup)
;;; enfors-org-setup.el ends here
