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
  (setq dashboard-startup-banner 'official) ;; Options: 'official, 'logo, or path to .png
  (setq dashboard-center-content t)         ;; Center everything
  (setq dashboard-show-shortcuts nil)       ;; Hide default help text at bottom

  ;; 2. What sections to show?
  (setq dashboard-items '((agenda . 10)     ;; Scheduled tasks & habits
                          (recents . 5)))   ;; Recent files

  ;; 3. Icons (Optional - requires all-the-icons font installed)
  ; (setq dashboard-set-heading-icons t)
  ; (setq dashboard-set-file-icons t)

  ;; 4. Custom navigation shortcuts (the "jump" keys)
  ;; We hook into the dashboard mode map to define keys that only work here.
  (add-hook 'dashboard-mode-hook
            (lambda ()
              ;; Jump to Inbox (i)
              (local-set-key (kbd "i") (lambda () (interactive)
                                         (find-file "~/devel/RoamNotes/20260124144908-inbox.org")))
              (local-set-key (kbd "w") (lambda () (interactive)
                                         (find-file "~/devel/RoamNotes/20220831105115-afry_todos.org")))
              (local-set-key (kbd "p") (lambda () (interactive)
                                         (find-file "~/devel/RoamNotes/20240808163532-springhaven_pathfinder_campaign.org"))))))

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
                       (org-agenda-sorting-strategy '(priority-down effort-up))))))

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
