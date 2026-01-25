(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/devel/RoamNotes")
  
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; --- DAILIES KEYBINDINGS ---
         ("C-c n d" . org-roam-dailies-capture-today)    ; Quick capture to today's log
         ("C-c n D" . org-roam-dailies-goto-today)       ; Open today's log file
         ("C-c n m" . org-roam-dailies-capture-tomorrow) ; Plan for tomorrow
         ("C-c n y" . org-roam-dailies-goto-yesterday))  ; Review yesterday

  :config
  ;; Enable the database synchronization
  (org-roam-db-autosync-mode)

  ;; --- DAILIES CONFIGURATION ---
  (require 'org-roam-dailies)
  
  ;; 1. Where do the files live? (Relative to org-roam-directory)
  ;; This puts them in ~/devel/RoamNotes/daily/
  (setq org-roam-dailies-directory "daily/")

  ;; 2. Template: Standard YYYY-MM-DD.org files
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n")
           :empty-lines 1))))

(provide 'enfors-roam-setup)
