;;; enfors-ttrpg-hangout-setup.el --- code for updating ttrpg-hangout.com
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'json)
(require 'seq) ;; For seq-filter

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

(defun ttrpg-hangout-articles-checklist ()
  "Insert a checklist of all published article titles at point."
  (interactive)
  (let ((articles (ttrpg-hangout-get-articles)))
    (dolist (article articles)
      (insert (format "- [ ] %s\n" (alist-get 'title article))))))

(defun ttrpg-hangout-update ()
  "Extract publishable TTRPG articles and metadata using the Org-Roam API."
  (interactive)
  (let ((manifest-data (ttrpg-hangout-get-articles))
        (json-encoding-pretty-print t))
    
    (with-temp-file "~/devel/RoamNotes/TTRPG-Hangout/manifest.json"
      (insert (json-encode manifest-data)))
    
    (message "Successfully exported %d articles to manifest.json" (length manifest-data))))

(provide 'enfors-ttrpg-hangout-setup)
;;; enfors-ttrpg-hangout-setup.el ends here
