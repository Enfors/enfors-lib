;;;; enfors-lib/define.el

(defun enf-ins-review-checklist ()
  "Insert a #define ticket review checklist at point."
  (interactive)
  (insert "- [/] Checklist
  - [ ] Read comments
  - [ ] Review changesets
  - [ ] Fill out questionnaire
  - [ ] Update ticket\n"))



