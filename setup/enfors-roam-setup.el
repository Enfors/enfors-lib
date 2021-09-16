(use-package org-roam
             :ensure t
             :init
             (setq org-roam-v2-ack t
;                   org-roam-db-update-method 'immediate
                   org-roam-completion-everywhere t
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
                    ("C-M-i"   . completion-at-point)
                    :map org-roam-dailies-map
                    ("Y" . org-roam-dailies-capture-yesterday)
                    ("T" . org-roam-dailies-capture-tomorrow))
             :bind-keymap
             ("C-c n d" . org-roam-dailies-map)
             :config
             (require 'org-roam-dailies)  ;; Ensure keymap available
             (org-roam-setup))

(provide 'enfors-roam-setup)
