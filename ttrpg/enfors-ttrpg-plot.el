;;; enfors-ttrpg-plot.el --- Plot Generator Module  -*- lexical-binding: t; -*-

(require 'enfors-ttrpg-core)

(defcustom enfors-ttrpg-plot-file "~/devel/RoamNotes/20250725230801-plot_generator.org"
  "Path to the Org-roam file containing Plot Generator tables."
  :type 'file
  :group 'enfors-ttrpg)

(defun enfors-ttrpg-plot-generate ()
  "Generate a 3-part plot and display it."
  (interactive)
  (let ((goal (enfors-ttrpg-roll-random-row enfors-ttrpg-plot-file "plot-goal"))
        (focus (enfors-ttrpg-roll-random-row enfors-ttrpg-plot-file "plot-focus"))
        (obstacle (enfors-ttrpg-roll-random-row enfors-ttrpg-plot-file "plot-obstacle")))
    (message "📜 Plot: %s %s, but %s." goal focus obstacle)))

(defun enfors-ttrpg-plot-insert ()
  "Generate a plot and insert it into the current buffer."
  (interactive)
  (let ((goal (enfors-ttrpg-roll-random-row enfors-ttrpg-plot-file "plot-goal"))
        (focus (enfors-ttrpg-roll-random-row enfors-ttrpg-plot-file "plot-focus"))
        (obstacle (enfors-ttrpg-roll-random-row enfors-ttrpg-plot-file "plot-obstacle")))
    (insert (format "**Plot Hook:** %s %s, but %s.\n" goal focus obstacle))))

(provide 'enfors-ttrpg-plot)
