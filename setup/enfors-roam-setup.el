(use-package org-roam
             :ensure t
             :init
             (setq org-roam-v2-ack t
                   org-roam-db-update-method 'immediate
                   org-roam-node-display-template "${title}")
             ; With tags, this should be (according to my Discourse thread:
;                   org-roam-node-display-template "${title} ${tag}")
             :custom
             (org-roam-directory "~/devel/RoamNotes")
             ;(org-roam-completion-everywhere t)
             :bind (("C-c n l" . org-roam-buffer-toggle)
                    ("C-c n f" . org-roam-node-find)
                    ("C-c n i" . org-roam-node-insert)
                    :map org-mode-map
                    ("C-M-i"   . completion-at-point))
             :config
             (org-roam-setup))

(provide 'enfors-roam-setup)
