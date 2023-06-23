(require 'org-element)

(defun urpg-random-item (items)
  (nth (random (length items)) items))

;; The following function was written by user dalanicolai:
;; https://emacs.stackexchange.com/questions/76421/how-to-get-item-names-from-lists-in-an-org-mode-file-using-elisp
(defun org-get-heading-random-list-elements (heading-title)
  (let* ((heading (car (org-element-map (org-element-parse-buffer)
                           'headline
                         (lambda (h)
                           (when (string= (org-element-property :raw-value h) heading-title)
                             h)))))
         (heading-contents (org-element-contents heading))
         (first-plain-list (caar (org-element-map heading-contents
                                     'section
                                   (lambda (s)
                                     (org-element-map s 'plain-list #'identity)))))
         (item-contents (org-element-map (org-element-contents first-plain-list)
                            'item
                          #'org-element-contents))
         (paragraph-contents (mapcar (lambda (ic)
                                       (org-element-map
                                           ic
                                           'paragraph
                                         #'org-element-contents))
                                     item-contents))
         (list-items (mapcar #'caar paragraph-contents)))
    (string-trim-right (substring-no-properties (nth (random (length list-items)) list-items)))))

(defun urpg-get-random-list-element-from-file-and-heading (file-name heading-title)
  (save-excursion
    (with-temp-buffer
      (insert-file-contents file-name)
      (org-get-heading-random-list-elements heading-title))))

(urpg-get-random-list-element-from-file-and-heading "~/test-file.org" "Heading1-1")

(setq urpg-core-file-name "~/devel/RoamNotes/20230112201948-unified_rpg_core_rules.org")

(defun urpg-get-random-item-from-core-heading (heading)
  (interactive)
  (urpg-get-random-list-element-from-file-and-heading urpg-core-file-name heading))
  

(defun urpg-get-random-action ()
  (interactive)
  (urpg-get-random-item-from-core-heading "Action"))

(defun urpg-get-random-theme ()
  (interactive)
  (urpg-get-random-item-from-core-heading "Theme"))

(defun urpg-get-random-action-and-theme ()
  (interactive)
  (let ((action (urpg-get-random-action))
        (theme (urpg-get-random-theme)))
    (message "Action and Theme: %s %s" action theme)
    (concat action " " theme)))

(urpg-get-random-action)
(urpg-get-random-theme)
(urpg-get-random-action-and-theme)
