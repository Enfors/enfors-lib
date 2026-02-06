(require 'enfors-org-setup)
(require 'enfors-roam-setup)

(use-package org
  :ensure t
  :config
  ;; --- 1. MODULES & LOADING (Must happen first) ---
  (add-to-list 'org-modules 'org-habit)
  (require 'org-habit)

  ;; --- 2. VISUAL TWEAKS ---
  (setq org-ellipsis "▾")
  (setq org-hide-emphasis-markers t)
  
  ;; --- 3. HABIT CONFIGURATION ---
  (setq org-habit-show-habits-only-for-today t)
  (setq org-habit-graph-column 60)
  (setq org-log-done 'time) 
  
  ;; --- 4. PRIORITIES ---
  (setq org-priority-start-cycle-with-default nil)
  (setq org-agenda-fontify-priorities t)

  ;; --- 5. TAGS ---
  (setq org-tag-alist
        '((:startgroup)
          ("@work" . ?w)
          ("@home" . ?h)
          (:endgroup)
          ("ttrpg" . ?t)
          ("starter" . ?s)
          ("urgent" . ?u))
        org-tag-faces
        '(("starter" . (:foreground "#449977" :weight bold))  ; Inviting Green
          ("urgent"  . (:foreground "#cc6666" :weight bold))  ; Alert Red
          ("@work"   . (:foreground "#6699cc"))))

  ;; --- 6. AGENDA FILES ---
  ;; We add both files here in the main block
  ;; Note - no point in doing it here. Resets from custom in .emacs anyway.
  ;(add-to-list 'org-agenda-files "~/devel/RoamNotes/20260124144908-inbox.org")
  ;(add-to-list 'org-agenda-files "~/devel/RoamNotes/20260124205807-habits.org")

  ;; --- 7. CAPTURE TEMPLATES ---
  (add-to-list 'org-capture-templates
               '("i" "Inbox Task" entry
                 (file "~/devel/RoamNotes/20260124144908-inbox.org")
                 "* TODO %?\n  %i"
                 :empty-lines 1))
                 
  (add-to-list 'org-capture-templates
               '("h" "Habit" entry
                 (file "~/devel/RoamNotes/20260124205807-habits.org")
                 "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a .+1d>>\n:PROPERTIES:\n:STYLE: habit\n:END:\n  %i"
                 :empty-lines 1))
  ;; --- 8. REFILE SETTINGS ---
  ;; Tell org-mode that any file in 'org-agenda-files' is a target.
  ;; :maxlevel 3 means you can refile to a Top Level heading, or 2 levels down.
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 3))))

  ;; Use full outline paths (e.g., "work.org/ProjectA/Task")
  (setq org-refile-use-outline-path 'file)

  ;; Essential for Helm/Ivy: Allows you to type "ProjA" and jumpt straight there
  ;; instead of having to tab-complete step-by-step.
  (setq org-outline-path-complete-in-steps t)

  ;; Allow creating new nodes during refile if you need a new parent
  (setq org-refile-allow-creating-parent-nodes 'confirm))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)

  ;; 1. Visual Configuration
  (setq dashboard-banner-logo-title "Welcome back, Operator.")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content t)  ;; Centering is ON
  (setq dashboard-show-shortcuts t)
  (setq org-agenda-window-setup 'current-window)

  ;; Display 'fortune' if installed
  (setq dashboard-footer-messages
        (if (executable-find "fortune")
            (list (string-trim (shell-command-to-string "fortune -s")))
          '("Fortune command not found. Install it for wisdom.")))
  
  ;; Tweak this prefix. If it's too long, it pushes the graph away.
  (setq dashboard-agenda-prefix-format " %-12:c %-10s ")

  ;; 2. DEFINE CUSTOM SECTIONS
  ;; (add-to-list 'dashboard-item-generators
  ;;              '(important . (lambda (list-size)
  ;;                              (insert (propertize "★ Critical Tasks" 'face 'dashboard-heading))
  ;;                              (insert "\n")
  ;;                              (let ((items (org-map-entries
  ;;                                            (lambda () 
  ;;                                              (org-get-heading t nil nil nil))
  ;;                                            "PRIORITY=\"A\"/TODO"
  ;;                                            'agenda)))
  ;;                                (if items
  ;;                                    (dolist (item items)
  ;;                                      (insert (format "  • %s\n" item)))
  ;;                                  (insert "  (No critical tasks right now)\n"))))))

  (add-to-list 'dashboard-item-generators
               '(shortcuts . (lambda (list-size)
                               (insert (propertize "Shortcuts:" 'face 'dashboard-heading))
                               (insert "\n")
                               (insert "  [i] Inbox   [w] Work   [c] Calendar   [C-c c] Capture   [d] Workflow doc"))))
   
  ;; 3. What sections to show?
  (setq dashboard-items '(
                          ;; (important . 10)
                          (agenda . 15)
                          (recents . 10)
                          (shortcuts . 2)))

  ;; 4. Sorting
  (setq dashboard-agenda-sort-strategy '(time-up priority up))

  ;; 5. Custom Navigation
  (add-hook 'dashboard-mode-hook
            (lambda ()
              (local-set-key (kbd "i") (lambda () (interactive) (find-file "~/devel/RoamNotes/20260124144908-inbox.org")))
              (local-set-key (kbd "w") (lambda () (interactive) (find-file "~/devel/RoamNotes/20220831105115-afry_todos.org")))
              (local-set-key (kbd "p") (lambda () (interactive) (find-file "~/devel/RoamNotes/20240808163532-springhaven_pathfinder_campaign.org")))
              (local-set-key (kbd "d") (lambda () (interactive) (find-file "~/devel/RoamNotes/20260131135628-org_roam_workflow.org")))
              (local-set-key (kbd "c") (lambda () (interactive) (find-file "~/devel/RoamNotes/20260131184817-calendar.org")))
              )))
  

(use-package org-agenda
  :ensure nil
  :config
  (setq org-agenda-custom-commands
       '(("w" "Work view"
          ((tags-todo "+work|+@work"
                      ((org-agenda-overriding-header "Work Tasks")
                       (org-agenda-sorting-strategy '(priority-down time-up))))))
         ("p" "Personal view"
          ((tags-todo "+personal"
                      ((org-agenda-overriding-header "Personal Tasks")))
           (tags-todo "+pf-prep"
                      ((org-agenda-overriding-header "Pathfinder Prep")))
           (tags-todo "+solo"
                      ((org-agenda-overriding-header "Solo Campaigns")))))
         ("n" "Process Inbox"
          ((tags "ALL"
                 ((org-agenda-files '("~/devel/RoamNotes/20260124144908-inbox.org"))
                  (org-agenda-overriding-header "Inbox items to refile"))))))))

;; Configure M-x calendar
;; Add week number display to M-x calendar
(setq calendar-intermonth-header
      (propertize "Wk" 'font-lock-face 'font-lock-keyword-face))

(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-comment-face))

(global-set-key (kbd "C-c h") 'dashboard-open)

(provide 'enfors-prod-setup)
