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
         ("C-c n D" . enfors-dailies-goto-today-smart)   ; Open today's log file
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
  ;; (setq org-roam-dailies-capture-templates
  ;;       '(("w" "Work Log" entry
  ;;          "* %? :work:"
  ;;          :target (file+head "%<%Y-%m-%d>.org"
  ;;                             "#+title: %<%Y-%m-%d>\n")
  ;;          :empty-lines 1)
          
  ;;         ("p" "Personal Diary" entry
  ;;          "* %? :personal:"
  ;;          :target (file+head "%<%Y-%m-%d>.org"
  ;;                             "#+title: %<%Y-%m-%d>\n")
  ;;          :empty-lines 1)))

  ;; 3. Custom functions
  (defun enfors-dailies-goto-today-smart ()
    "Open today's daily note instantly if it exists.
     If it does not exist, ask which template to use to create it."
    (interactive)
    (let* ((dailies-dir (expand-file-name org-roam-dailies-directory org-roam-directory))
           (today-file (expand-file-name (format-time-string "%Y-%m-%d.org") dailies-dir)))
      (if (file-exists-p today-file)
          (find-file today-file)
        ;; File doesn't exist? Trigger the capture menu to create it.
        (org-roam-dailies-capture-today)))))

(defun enfors-calendar-open-roam-daily ()
  "Open the Org-roam daily note in the OTHER window, keeping Calendar visible."
  (interactive)
  (require 'org-roam-dailies)
  
  (let* ((cal-buffer (current-buffer)) ;; Remember the Calendar buffer
         (date (calendar-cursor-to-date t))
         (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
    
    ;; 1. Open the daily (this replaces the Calendar in the current window)
    (org-roam-dailies--capture time t)
    
    ;; 2. The Daily is now the current buffer. Let's move it.
    (let ((daily-buffer (current-buffer)))
      ;; Restore the Calendar to this window
      (switch-to-buffer cal-buffer)
      ;; Open the Daily in the 'other' window (and focus it)
      (switch-to-buffer-other-window daily-buffer))))

;; Bind it
(define-key calendar-mode-map (kbd "d") 'enfors-calendar-open-roam-daily)

(provide 'enfors-roam-setup)
