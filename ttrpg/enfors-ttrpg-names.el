;;; enfors-ttrpg-names.el --- Name Generator Module  -*- lexical-binding: t; -*-

(require 'enfors-ttrpg-core)

;; =============================================================================
;; CONFIGURATION
;; =============================================================================

(defcustom enfors-ttrpg-names-file "~/devel/RoamNotes/20251123101302-name_generator.org"
  "Path to the Org file containing Name tables."
  :type 'file
  :group 'enfors-ttrpg)

;; =============================================================================
;; LOGIC
;; =============================================================================

(defun enfors-ttrpg-names-get-male ()
  "Generate and display a Male name."
  (interactive)
  (let ((name (enfors-ttrpg-roll-random-row enfors-ttrpg-names-file "names-male")))
    (message "👤 Name (Male): %s" name)
    name))

(defun enfors-ttrpg-names-get-female ()
  "Generate and display a Female name."
  (interactive)
  (let ((name (enfors-ttrpg-roll-random-row enfors-ttrpg-names-file "names-female")))
    (message "👤 Name (Female): %s" name)
    name))

(defun enfors-ttrpg-names-insert-male ()
  "Insert a random Male name at point."
  (interactive)
  (insert (enfors-ttrpg-roll-random-row enfors-ttrpg-names-file "names-male")))

(defun enfors-ttrpg-names-insert-female ()
  "Insert a random Female name at point."
  (interactive)
  (insert (enfors-ttrpg-roll-random-row enfors-ttrpg-names-file "names-female")))

(provide 'enfors-ttrpg-names)
