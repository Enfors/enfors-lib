;;; rpg.el --- Utilities for roleplaying games
;;; Commentary:

;;; Code:
(require 'org)

(defun org-random-entry ()
  (interactive)
  (let ((items nil)
        (selected-item nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*[-+*]\\s-+" nil t)
        (let ((start (point))
              (end (save-excursion
                     (if (re-search-forward "^\\s-*[-+*]\\s-+" nil t)
                         (match-beginning 0)
                       (point-max)))))
          (push (buffer-substring-no-properties start end) items))))
    (when items
      (setq selected-item (nth (random (length items)) items))
      (setq items nil)
      selected-item)))

(defun org-random-entry-message ()
  (interactive)
  (message "Random item: %s" (org-random-entry)))

(defun org-get-random-item (filename)
  "Select a random item from an org-mode list in the given file."
  (with-temp-buffer
    (insert-file-contents filename)
    (org-mode)
    (org-element-map (org-element-parse-buffer) 'item
      (lambda (item)
        (org-element-property :raw-value item)))
    (let ((items (org-element-map (org-element-parse-buffer) 'item
                    (lambda (item)
                      (org-element-property :raw-value item)))))
      (nth (random (length items)) items))))

(provide 'rpg)
;; Usage example:
(message "Random item: %s" (org-get-random-item "file.org"))
;;; rpg.el ends here
