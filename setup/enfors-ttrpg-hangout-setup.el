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

                           ;; Get basename of filename
                           (file-name (file-name-nondirectory
                                       (org-roam-node-file node)))

                           ;; Get custom properties
                           (summary      (cdr (assoc "SUMMARY" props)))
                           (author       (or (cdr (assoc "AUTHOR" props))
                                             "Christer Enfors"))
                           (publish-date (cdr (assoc "PUBLISH-DATE" props))))
                      `((id           . ,id)
                        (title        . ,title)
                        (file-name    . ,file-name)
                        (summary      . ,(or summary ""))
                        (author       . ,author)
                        (publish-date . ,(or publish-date ""))
                        (tags         . ,tags))))
                  publish-nodes)))
    articles))

(defun ttrpg-hangout-get-article-by-title (target-title)
  "Return the metadata alist for the article matching TARGET-TITLE."
  (seq-find (lambda (article)
              (string= (alist-get 'title article) target-title))
            (ttrpg-hangout-get-articles)))

(defun ttrpg-hangout-articles-checklist ()
  "Insert a checklist of all published article titles at point."
  (interactive)
  (let ((articles (ttrpg-hangout-get-articles)))
    (dolist (article articles)
      (insert (format "- [ ] %s\n" (alist-get 'title article))))))

(defun ttrpg-hangout-make-manifest ()
  "Extract publishable TTRPG articles and metadata using the Org-Roam API."
  (interactive)
  (let ((manifest-data (ttrpg-hangout-get-articles))
        (json-encoding-pretty-print t))
    
    (with-temp-file "~/devel/RoamNotes/TTRPG-Hangout/manifest.json"
      (insert (json-encode manifest-data)))
    
    (message "Successfully exported %d articles to manifest.json" (length manifest-data))))

(defun ttrpg-hangout-update ()
  "Update the local copy of the HTML files."
  (interactive)
  (call-interactively #'ttrpg-hangout-make-manifest)

  ;; Run update.sh
  (let ((output-buffer (get-buffer-create "*TTRPG-Hangout Update*"))
        (script-path (expand-file-name "~/devel/RoamNotes/update.sh")))

    ;; Clear the buffer of any previous output
    (with-current-buffer output-buffer
      (erase-buffer))

    ;; Run the process.
    ;; nil = no input file
    ;; output-buffer = destination
    ;; t = update display as output arrives
    (call-process script-path nil output-buffer t)

    ;; Pop open the window so you can read the log
    (pop-to-buffer output-buffer)))

(provide 'enfors-ttrpg-hangout-setup)
;;; enfors-ttrpg-hangout-setup.el ends here
