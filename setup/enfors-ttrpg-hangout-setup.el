;;; enfors-ttrpg-hangout-setup.el --- code for updating ttrpg-hangout.com
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-roam)
(require 'json)
(require 'seq) ;; For seq-filter

(defun ttrpg-hangout-get-articles ()
  "Return a list of all articles marked for publication."
  (interactive)
  (let* ((all-nodes (org-roam-node-list))
         (publish-nodes (seq-filter (lambda (node)
                                      (member "publish-ttrpg-hangout"
                                              (org-roam-node-tags node)))
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
                             (replace-regexp-in-string
                              "^[0-9]+-" ""
                              (file-name-sans-extension
                               (file-name-nondirectory
                                (org-roam-node-file node))))
                            ".html"))

                           ;; Get custom properties
                           (summary      (cdr (assoc "SUMMARY" props)))
                           (author       (or (cdr (assoc "AUTHOR" props))
                                             "Christer Enfors"))
                           (publish-date (cdr (assoc "PUBLISH-DATE" props)))
                           (update-date  (or (cdr (assoc "UPDATE-DATE" props))
                                             publish-date)))
                      `((id           . ,id)
                        (title        . ,title)
                        (file-name    . ,file-name)
                        (summary      . ,(or summary ""))
                        (author       . ,author)
                        (publish-date . ,(or publish-date nil))
                        (update-date  . ,(or update-date nil))
                        (tags         . ,tags))))
                  publish-nodes)))
    articles))

(defun ttrpg-hangout-get-real-articles ()
  "Return a list of all non-meta articles marked for publication."
  (seq-filter (lambda (article)
                (> (length (alist-get 'publish-date article)) 0))
              (ttrpg-hangout-get-articles)))
(defun ttrpg-hangout-sort-articles (articles)
  "Sort the alist of ARTICLES by publish-date, newest first."
  (seq-sort (lambda (a b)
              (string> (alist-get 'publish-date a)
                       (alist-get 'publish-date b)))
            articles))
(defun ttrpg-hangout-sort-articles-update (articles)
  "Sort the alist of ARTICLES by update-date, newest first."
  (seq-sort (lambda (a b)
              (string> (alist-get 'update-date a)
                       (alist-get 'update-date b)))
            articles))

(defun ttrpg-hangout-make-recent-html (articles &optional limit)
  "Generate an HTML snippet of the most recent ARTICLES. LIMIT defaults to 10."
  (let* ((max (or limit 10))
         (sorted (seq-filter (lambda (article)
                               (> (length (alist-get 'publish-date article)) 0))
                             (ttrpg-hangout-sort-articles articles)))
         (recent (seq-take sorted max)))
    (with-temp-file "~/devel/RoamNotes/TTRPG-Hangout/recent.html"
      (dolist (article recent)
        (let* ((title        (alist-get 'title        article))
               (file-name    (alist-get 'file-name    article))
               (publish-date (alist-get 'publish-date article)))
          (insert (format "<li><a href=\"%s\">%s</a> (%s)</li>\n"
                            file-name title publish-date)))))
    (insert (format "Successfully put %d articles in recent.html.\n"
                    (length recent)))))

(defun ttrpg-hangout-read-file (file-path)
  "Return the contents of FILE-PATH as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun ttrpg-hangout-make-atom-feed (atom-file-name articles)
  "Generate atom file called ATOM-FILE-NAME from ARTICLES."
  (with-temp-file atom-file-name
    (let ((now (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t)))
      (insert (format "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <title>TTRPG-Hangout</title>
  <link href=\"https://ttrpg-hangout.com/atom.xml\" rel=\"self\"/>
  <link href=\"https://ttrpg-hangout.com/\"/>
  <icon>https://ttrpg-hangout.com/images/feed-icon.png</icon>
  <logo>https://ttrpg-hangout.com/images/feed-logo.png</logo>
  <id>https://ttrpg-hangout.com/</id>
  <updated>%sT00:00:00Z</updated>
  <author>
    <name>Christer Enfors</name>
  </author>\n\n" now)))
    (dolist (article articles)
      (when (alist-get 'publish-date article)
        (let* ((title        (alist-get 'title        article))
               (file-name    (alist-get 'file-name    article))
               (html         (ttrpg-hangout-read-file
                              (format "~/devel/RoamNotes/TTRPG-Hangout/%s"
                                      file-name)))
               (publish-date (alist-get 'publish-date article))
               (update-date  (alist-get 'update-date  article)))
          (setq html
                (replace-regexp-in-string "<img src=\"/"
                                          "<img src=\"https://ttrpg-hangout.com/"
                                          html))
          (insert (format "  <entry>
    <title>%s</title>
    <link href=\"https://ttrpg-hangout.com/%s\"/>
    <id>https://ttrpg-hangout.com/%s</id>
    <published>%s</published>
    <updated>%s</updated>
    <summary>Summary not available.</summary>
    <content type=\"html\"><![CDATA[
      %s
    ]]></content>
  </entry>\n" title file-name file-name publish-date update-date html)))))
    (insert "</feed>\n"))
  (insert (format "Successfully put %d articles in atom.xml.\n"
                  (length articles))))

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
      (insert (format "1. [ ] %s\n" (alist-get 'title article)))))
  (message "recent.html successfully updated."))

(defun ttrpg-hangout-make-manifest (articles)
  "Write meta-data of ARTICLES in manifest.json."
  (let ((json-encoding-pretty-print t))
    (with-temp-file "~/devel/RoamNotes/TTRPG-Hangout/manifest.json"
      (insert (json-encode articles)))
    
    (insert (format "Successfully exported %d articles to manifest.json.\n"
                    (length articles)))))

(defun ttrpg-hangout-update ()
  "Update the local copy of the HTML files."
  (interactive)
  (let ((articles (ttrpg-hangout-get-real-articles))
        (output-buffer (get-buffer-create "*TTRPG-Hangout Update*"))
        (script-path (expand-file-name "~/devel/RoamNotes/update.sh")))
    ;; Run the update.sh script.
    ;; Clear the buffer of any previous output
    (with-current-buffer output-buffer
      (let ((inhibit-read-only 1))
        (erase-buffer)
        (special-mode)
        (pop-to-buffer output-buffer)
        ;; Generate manifest.json
        (ttrpg-hangout-make-manifest articles)

        ;; Generate recent.html
        (ttrpg-hangout-make-recent-html articles)

        ;; Run update.sh.
        ;; nil = no input file
        ;; output-buffer = destination
        ;; t = update display as output arrives
        (call-process script-path nil output-buffer t)
      
        ;; Generate atom.xml
        (ttrpg-hangout-make-atom-feed
         "~/devel/RoamNotes/TTRPG-Hangout/atom.xml"
         ;; Max 20 in feed
         (seq-take (ttrpg-hangout-sort-articles-update articles) 20)))))

  (message "Local TTRPG-Hangout update completed."))

(provide 'enfors-ttrpg-hangout-setup)
;;; enfors-ttrpg-hangout-setup.el ends here
