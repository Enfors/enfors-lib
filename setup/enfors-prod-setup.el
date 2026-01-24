(require 'enfors-org-setup)
(require 'enfors-roam-setup)

(use-package org
  :ensure t
  :config
  ;; 1. Visual Tweaks
  (setq org-ellipsis "▾")             ;; Replaces "..." with an arrow
  (setq org-hide-emphasis-markers t)  ;; Hides markers, // becomes italic

  ;; 2. Enable Habits Module
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)    ;; Adjusts where consistency graph starts

  ;; Make sure priorities are colorful and standard
  (setq org-priority-start-cycle-with-default nil)
  (setq org-agenda-fontify-priorities t)

  ;; 3. Define Context Tags
  ;; The character after the dog is the shortcut key for that tag
  (setq org-tag-alist
        '((:startgroup)
          ;; Mutually exclusive tags (You can't be at work and at home)
          ("@work" . ?w)
          ("@home" . ?h)
          (:endgroup)
          ;; General tags
          ("ttrpg" . ?t)
          ("pr-prep" . ?p)
          ("solo" . ?s)
          ("urgent" . ?u))))

(use-package org
  :ensure nil
  :config
  ;; 1. Add the inbox file to the agenda files.
  (add-to-list 'org-agenda-files "~/devel/RoamNotes/20260124144908-inbox.org")

  ;; 2. The "Universal Inbox" Capture Template
  ;; We use add-to-list to preserve any existing templates.
  (add-to-list 'org-capture-templates
               '("i" "Inbox Task" entry
                 (file "~/devel/RoamNotes/20260124144908-inbox.org")
                 "* TODO %?\n  %i"
                 :empty-lines 1)))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)

  ;; 1. Visual Configuration
  (setq dashboard-banner-logo-title "Welcome back, Operator.")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-agenda-prefix-format "%-12:c %-10s ")

  ;; 2. DEFINE CUSTOM SECTIONS (The Robust Manual Way)
  (add-to-list 'dashboard-item-generators
               '(important . (lambda (list-size)
                               ;; A. Insert the Header manually
                               ;; We use the standard dashboard face for consistency
                               (insert (propertize "★ Critical Tasks" 'face 'dashboard-heading))
                               (insert "\n")
                               
                               ;; B. Fetch the items
                               (let ((items (org-map-entries
                                             (lambda () 
                                               ;; (org-get-heading NO-TAGS NO-TODO NO-PRIORITY NO-COMMENT)
                                               ;; We pass 't' only to tags. We WANT Todo and Priority.
                                               (org-get-heading t nil nil nil))
                                             "PRIORITY=\"A\"/TODO" ;; Filter
                                             'agenda)))            ;; Scope
                                 
                                 ;; C. Insert them
                                 (if items
                                     (dolist (item items)
                                       ;; Simple, clean insertion
                                       (insert (format "  • %s\n" item)))
                                   (insert "  (No critical tasks right now)\n"))))))

  (add-to-list 'dashboard-item-generators
               '(shortcuts . (lambda (list-size)
                               (insert (propertize "Shortcuts:" 'face 'dashboard-heading))
                               (insert "\n")
                               (insert "  [i] Inbox   [w] Work view    [p] Personal view"))))
  
  ;; 3. What sections to show?
  (setq dashboard-items '((important . 10)   ;; Custom section first
                          (agenda . 10)
                          (shortcuts . 5)
                          (recents . 5)))   

  ;; 4. Sorting Strategy (for the standard agenda section)
  (setq dashboard-agenda-sort-strategy '(priority-up time-up))

  ;; 5. Custom Navigation Shortcuts
  (add-hook 'dashboard-mode-hook
            (lambda ()
              (local-set-key (kbd "i") (lambda () (interactive)
                                       (find-file "~/devel/RoamNotes/20260124144908-inbox.org")))
              (local-set-key (kbd "w") (lambda () (interactive)
                                       (find-file "~/devel/RoamNotes/20220831105115-afry_todos.org")))
              (local-set-key (kbd "p") (lambda () (interactive)
                                         (find-file "~/devel/RoamNotes/20240808163532-springhaven_pathfinder_campaign.org")))))
  ;; 6. Auto-Focus the Agenda
  ;; When dashboard opens, jump straight to the first actionable item
  (add-hook 'dashboard-mode-hook
            (lambda ()
              (goto-char (point-min))       ;; Start at top
              (if (search-forward "Agenda for" nil t) ;; Find the Agenda Header
                  (progn
                    (forward-line 1)        ;; Go down one line
                    (back-to-indentation))  ;; Move to start of text
                (goto-char (point-min)))))) ;; Fallback to top if not found

(use-package org-agenda
  :ensure nil
  :config
  (setq org-agenda-custom-commands
       '(
         ;; 1. Work view (press 'w')
         ;; Shows tasks tagged 'work' or '@work'
         ("w" "Work view"
          ((tags-todo "+work|+@work"
                      ((org-agenda-overriding-header "Work Tasks")
                       (org-agenda-sorting-strategy '(priority-down time-up))))))

         ;; 2. Personal view (press 'p')
         ("p" "Personal view"
          ((tags-todo "+personal"
                      ((org-agenda-overriding-header "Personal Tasks")))
           (tags-todo "+pf-prep"
                      ((org-agenda-overriding-header "Pathfinder Prep")))
           (tags-todo "+solo"
                      ((org-agenda-overriding-header "Solo Campaigns")))))

         ;; 3. Inbox processor (press 'p')
         ;; Shows everything in your inbox file specifically
         ("n" "Process Inbox"
          ((tags "ALL"
                 ((org-agenda-files '("~/devel/RoamNotes/20260124144908-inbox.org"))
                  (org-agenda-overriding-header "Inbox items to refile"))))))))

(global-set-key (kbd "C-c h") 'dashboard-open)

(provide 'enfors-prod-setup)
