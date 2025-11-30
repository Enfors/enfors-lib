;; 1. The Engine: Elfeed
(use-package elfeed
  :ensure t  ;; Auto-install if missing
  :bind
  ("C-x w" . elfeed) ;; Bind key
  :config
  ;; Settings apply after the package loads
  (setq elfeed-search-filter "@2-months-ago +unread"
        elfeed-sort-order 'descending)

  ;; Visual tweaks
  ;; This sets the Title column to exactly 60 characters wide.
  ;; Short titles get padded, long titles get truncated.
  (setq elfeed-search-title-max-width 60
        elfeed-search-title-min-width 60)
  ;; Setting min=max forces a strict column grid.
  
  ;; Optional: Clamp the Feed Name column width too
  (setq elfeed-search-feed-max-width 20
        elfeed-search-feed-min-width 20))

;; 2. The Bridge: Elfeed-Org - configuration in org files
(use-package elfeed-org
  :ensure t  ; Auto-install if missing
  :after elfeed ;; Load this only after elfeed is ready
  :config
  ;; Point to your database file
  (setq rmh-elfeed-org-files (list "~/devel/RoamNotes/20251129102433-elfeed_subscriptions.org"))
  ;; Initialize the bridge
  (elfeed-org))

(provide 'enfors-elfeed-setup)
