(require 'org-element)
(defun urpg-get-headings-from-file (file)
  "Return a list of all headings from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (org-map-entries
     (lambda ()
       (let ((heading (org-element-at-point)))
         (when (eq (org-element-type heading) 'headline)
           (org-element-property :raw-value heading)))))))

(defun urpg-load-ast (file)
  "Load an org-file in the background and return its AST."
  (with-temp-buffer
    (insert-file-contents file)
    (org-element-parse-buffer)))

(defun urpg-find-first-list-under-heading (ast target-heading)
  (let (found-heading)
    (org-element-map ast 'headline
      (lambda (headline)
        (when (and (not found-heading)
                   (string= (org-element-property :raw-value headline)
                            target-heading))
          (setq found-heading t)
          (org-element-map headline 'plain-list
            (lambda (plain-list)
              plain-list)))))))

(defun urpg-extract-item-names (plain-list)
  (org-element-map plain-list 'item
    (lambda (item)
      (let ((item-content (org-element-contents item)))
        (when item-content
          (org-element-interpret-data (car item-content)))))))


(defun urpg-random-item (items)
  (nth (random (length items)) items))

;(setq ast (urpg-load-ast
;           "~/devel/RoamNotes/20230112201948-unified_rpg_core_rules.org"))
(setq ast (urpg-load-ast "~/test-file.org"))
(setq items (urpg-find-first-list-under-heading ast "Heading1"))

(org-element-property :bullet (car items))
(message (urpg-random-item items))

;; Experiments
(car (urpg-extract-item-names items))
(setq item-list (org-element-map items 'item
                  (lambda (item)
                    item)))
(car item-list)
(setq my-item (car item-list))
(car my-item)
(org-element-property :paragraph (car my-item))


(defun list-item-properties (item)
  (let ((props (nth 1 (assq (org-element-type item) org-element--set-regexps)))
        (properties-alist))
    (dolist (prop props)
      (let ((value (org-element-property prop item)))
        (when value
          (push (cons prop value) properties-alist))))
    properties-alist))

(list-item-properties my-item)
