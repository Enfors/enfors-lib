;;; enfors-ttrpg-hangout-setup.el --- code for updating ttrpg-hangout.com
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'json)
(require 'seq) ;; For seq-filter

(defun ttrpg-hangout-update ()
  "Extract publishable TTRPG articles and metadata using the Org-Roam API."
  (interactive)
  (let* (;; 1. Get ALL nodes from the API, then filter for our tag
         (all-nodes (org-roam-node-list))
         (publish-nodes (seq-filter (lambda (node)
                                      (member "publish-ttrpg-hangout" (org-roam-node-tags node)))
                                    all-nodes))
         
         ;; 2. Map over the filtered nodes and build our JSON structure
         (manifest-data
          (mapcar (lambda (node)
                    (let* ((id    (org-roam-node-id node))
                           (title (org-roam-node-title node))
                           (tags  (org-roam-node-tags node))
                           (props (org-roam-node-properties node))
                           
                           ;; Pluck custom properties
                           (summary      (cdr (assoc "SUMMARY" props)))
                           (author       (cdr (assoc "AUTHOR" props)))
                           (publish-date (cdr (assoc "PUBLISH-DATE" props))))
                      
                      `((id      . ,id)
                        (title   . ,title)
                        (summary . ,(or summary ""))
                        (author  . ,(or author "Christer Enfors"))
                        (tags    . ,tags))))
                  publish-nodes))
         
         (json-encoding-pretty-print t))
    
    ;; 3. Write to file
    (with-temp-file "~/devel/RoamNotes/TTRPG-Hangout/manifest.json"
      (insert (json-encode manifest-data)))
    
    (message "Successfully exported %d articles to manifest.json" (length manifest-data))))

(defun ttrpg-hangout-get-articles ()
  "Return a list of all articles marked for publication."
  (interactive)
  (let* ((all-nodes (org-roam-node-list))
         (publish-nodes (seq-filter (lambda (node)
                                      (member "publish-ttrpg-hangout" (org-roam-node-tags node)))
                                    all-nodes))
         (articles
          (mapcar (lambda (node)
                    (let* ((id    (org-roam-node-id         node))
                           (title (org-roam-node-title      node))
                           (tags  (org-roam-node-tags       node))
                           (props (org-roam-node-properties node))

                           ;; Get custom properties
                           (summary      (cdr (assoc "SUMMARY" props)))
                           (author       (or (cdr (assoc "AUTHOR" props))
                                             "Christer Enfors"))
                           (publish-date (cdr (assoc "PUBLISH_DATE" props))))
                      `((id           . ,id)
                        (title        . ,title)
                        (summary      . ,(or summary ""))
                        (author       . ,author)
                        (publish-date . ,(or publish-date ""))
                        (tags         . ,tags))))
                  publish-nodes)))
    articles))

(provide 'enfors-ttrpg-hangout-setup)
;;; enfors-ttrpg-hangout-setup.el ends here
