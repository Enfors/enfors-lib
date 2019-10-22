;;;; enfors-org-journal-setup.el - Set up org-journal
;;;; Get org-journal with M-x package-install org-journal.
;;;;
;;;; Add a new journal entry with C-c C-j.
;;;;
;;;; Entries are viewable from the calendar.
;;;; j:   view entry
;;;; C-j: view entry but don't switch to it
(require 'org-journal)
(setq org-journal-dir         "~/priv/journal/"
      org-journal-date-format "%x: %A")

(setq org-duration-format (quote h:mm))
