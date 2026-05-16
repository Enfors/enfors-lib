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
                           (file-name
                            (concat
                             (replace-regexp-in-string "^[0-9]+-" ""
                                                       (file-name-sans-extension
                                                        (file-name-nondirectory
                                                         (org-roam-node-file node))))
                            ".html"))

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

(defun ttrpg-hangout-sort-articles (articles)
  "Sort the alist of ARTICLES by publish-date, newest first."
  (seq-sort (lambda (a b)
              (string> (alist-get 'publish-date a)
                       (alist-get 'publish-date b)))
            articles))

(defun ttrpg-hangout-make-recent-html (articles &optional limit)
  "Generate an HTML snippet of the most recent ARTICLES. LIMIT defaults to 10."
  (let* ((max (or limit 10))
         (sorted (ttrpg-hangout-sort-articles articles))
         (recent (seq-take sorted max)))
    (with-temp-file "~/devel/RoamNotes/TTRPG-Hangout/recent.html"
      (dolist (article recent)
        (let* ((title        (alist-get 'title        article))
               (file-name    (alist-get 'file-name    article))
               (publish-date (alist-get 'publish-date article)))
          (unless (string-empty-p publish-date)
            (insert (format "<li><a href=\"%s\">%s</a> (%s)</li>\n"
                            file-name title publish-date))))))))

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
      (insert (format "- [ ] %s\n" (alist-get 'title article)))))
  (message "recent.html successfully generated."))

(defun ttrpg-hangout-make-manifest (articles)
  "Write meta-data of ARTICLES in manifest.json."
  (let ((json-encoding-pretty-print t))
    (with-temp-file "~/devel/RoamNotes/TTRPG-Hangout/manifest.json"
      (insert (json-encode articles)))
    
    (message "Successfully exported %d articles to manifest.json" (length articles))))

(defun ttrpg-hangout-update ()
  "Update the local copy of the HTML files."
  (interactive)
  (let ((articles (ttrpg-hangout-get-articles))
        (output-buffer (get-buffer-create "*TTRPG-Hangout Update*"))
        (script-path (expand-file-name "~/devel/RoamNotes/update.sh")))
    (ttrpg-hangout-make-manifest articles)
    (ttrpg-hangout-make-recent-html articles)

    ;; Run the update.sh script.
    ;; Clear the buffer of any previous output
    (with-current-buffer output-buffer
      (erase-buffer)

      ;; Run the process.
      ;; nil = no input file
      ;; output-buffer = destination
      ;; t = update display as output arrives
      (call-process script-path nil output-buffer t)
      (special-mode)

      ;; Pop open the window so you can read the log
      (pop-to-buffer output-buffer)))
  (message "Local TTRPG-Hangout update completed."))

(provide 'enfors-ttrpg-hangout-setup)
;;; enfors-ttrpg-hangout-setup.el ends here
