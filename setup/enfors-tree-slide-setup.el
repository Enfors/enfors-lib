;;; enfors-tree-slide-setup.el
(use-package org-tree-slide
  :ensure t
  :bind
  ;; Global keys to start the mode
  (("<f8>"   . org-tree-slide-mode)
   ("S-<f8>" . org-tree-slide-skip-done-toggle))

  ;; --- Specific keys for presentation mode ---
  :bind (:map org-tree-slide-mode-map
         ("C-<right>" . org-tree-slide-move-next-tree)
         ("C-<left>" . org-tree-slide-move-previous-tree)
         ("<next>" . org-tree-slide-move-next-tree)     ;; PageDown
         ("<prior>" . org-tree-slide-move-previous-tree) ;; PageUp
         ("C-x C-c" . org-tree-slide-content))           ;; Return to ToC view
  
  :config
  (org-tree-slide-simple-profile)
  
  (setq org-tree-slide-slide-in-effect t
        org-tree-slide-activate-message   "Presentation Mode ON"
        org-tree-slide-deactivate-message "Presentation Mode OFF"
        org-tree-slide-header-overlay-p nil)

  ;; --- Visual tweaks when presenting
  (add-hook 'org-tree-slide-play-hook
            (lambda ()
              (text-scale-set 2)           ;; Zoom in (Make text huge)
              (org-display-inline-images)  ;; Show images
              (read-only-mode 1)))         ;; Prevent accidental typing

  (add-hook 'org-tree-slide-stop-hook
            (lambda ()
              (text-scale-set 0)           ;; Reset zoom
              (read-only-mode 0))))        ;; Re-enable typing
(provide 'enfors-tree-slide-setup)
