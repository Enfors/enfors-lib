;;; enfors-ttrpg-npc.el --- NPC Generator Module  -*- lexical-binding: t; -*-

(require 'enfors-ttrpg-core)

;; =============================================================================
;; CONFIGURATION
;; =============================================================================

(defcustom enfors-ttrpg-npc-file "~/devel/RoamNotes/20250109111213-npc_generator.org"
  "Path to the Org-roam file containing NPC tables."
  :type 'file
  :group 'enfors-ttrpg)

;; =============================================================================
;; NPC SPECIFIC LOGIC
;; =============================================================================

(defun enfors-ttrpg-npc--lookup-ocean (table-name val)
  "Look up OCEAN trait in ENFORS-TTRPG-NPC-FILE using VAL (1-7)."
  (let* ((data (enfors-ttrpg-get-table enfors-ttrpg-npc-file table-name))
         (row-idx (random 4))
         (col-idx val))
    (nth col-idx (nth row-idx data))))

(defun enfors-ttrpg-npc--roll-size ()
  "Roll size, return list (Description Modifier)."
  (let* ((roll (1+ (random 20)))
         (result (cond ((= roll 1)   '("Very small" -3))
                       ((<= roll 3)  '("Small" -2))
                       ((<= roll 7)  '("Somewhat small" -1))
                       ((<= roll 13) '("Average" 0))
                       ((<= roll 17) '("Somewhat large" 1))
                       ((<= roll 19) '("Large" 2))
                       (t            '("Very large" 3)))))
    result))

(defun enfors-ttrpg-npc--lookup-body-part (size-mod)
  "Roll 2d4 + SIZE-MOD and return description."
  (let ((roll (+ 1 (random 4) 1 (random 4) size-mod)))
    (cond ((<= roll 1) "Extremely small")
          ((= roll 2)  "Very small")
          ((= roll 3)  "Small")
          ((= roll 4)  "Somewhat small")
          ((= roll 5)  "Average")
          ((= roll 6)  "Somewhat large")
          ((= roll 7)  "Large")
          ((= roll 8)  "Very large")
          (t           "Extremely large"))))

(defun enfors-ttrpg-npc--roll-feature ()
  "Roll feature recursively from ENFORS-TTRPG-NPC-FILE."
  (let ((roll (1+ (random 20))))
    (if (>= roll 15)
        (format "%s, %s" (enfors-ttrpg-npc--roll-feature) (enfors-ttrpg-npc--roll-feature))
      (let* ((data (enfors-ttrpg-get-table enfors-ttrpg-npc-file "app-features"))
             (row (nth (1- roll) data)))
        (if (> (length row) 1) (nth 1 row) (car row))))))

;; =============================================================================
;; MAIN GENERATOR COMMAND
;; =============================================================================

(defun enfors-ttrpg-npc-generate ()
  "Generate a full NPC using tables from `enfors-ttrpg-npc-file`."
  (interactive)
  (let* ((val-open (enfors-ttrpg-roll-d8-reroll-8))
         (val-conc (enfors-ttrpg-roll-d8-reroll-8))
         (val-extr (enfors-ttrpg-roll-d8-reroll-8))
         (val-agre (enfors-ttrpg-roll-d8-reroll-8))
         (val-neur (enfors-ttrpg-roll-d8-reroll-8))
         
         (trait-open (enfors-ttrpg-npc--lookup-ocean "ocean-openness" val-open))
         (trait-conc (enfors-ttrpg-npc--lookup-ocean "ocean-conscientiousness" val-conc))
         (trait-extr (enfors-ttrpg-npc--lookup-ocean "ocean-extroversion" val-extr))
         (trait-agre (enfors-ttrpg-npc--lookup-ocean "ocean-agreeableness" val-agre))
         (trait-neur (enfors-ttrpg-npc--lookup-ocean "ocean-neuroticism" val-neur))

         (goal (enfors-ttrpg-roll-random-row enfors-ttrpg-npc-file "agenda-goal"))
         (focus (enfors-ttrpg-roll-random-row enfors-ttrpg-npc-file "agenda-focus"))
         (obstacle (enfors-ttrpg-roll-random-row enfors-ttrpg-npc-file "agenda-obstacle"))

         (height (enfors-ttrpg-roll-random-row enfors-ttrpg-npc-file "app-height"))
         (size-data (enfors-ttrpg-npc--roll-size))
         (size-desc (nth 0 size-data))
         (size-mod  (nth 1 size-data))
         (eyes (enfors-ttrpg-roll-random-row enfors-ttrpg-npc-file "app-eyes"))
         (skin (enfors-ttrpg-roll-random-row enfors-ttrpg-npc-file "app-skin"))
         (hair-col (enfors-ttrpg-roll-random-row enfors-ttrpg-npc-file "app-hair-color"))
         (hair-len (enfors-ttrpg-roll-random-row enfors-ttrpg-npc-file "app-hair-length"))
         (hair-sty (enfors-ttrpg-roll-random-row enfors-ttrpg-npc-file "app-hair-style"))
         (facial (enfors-ttrpg-roll-random-row enfors-ttrpg-npc-file "app-facial-hair"))
         (features (enfors-ttrpg-npc--roll-feature)) 

         (chest (enfors-ttrpg-npc--lookup-body-part size-mod))
         (waist (enfors-ttrpg-npc--lookup-body-part size-mod))
         (bottom (enfors-ttrpg-npc--lookup-body-part size-mod)))

    (insert "| Personality | Value | Description | Appearance | Details |\n")
    (insert "|---+---+---+---+---|\n")
    (insert (format "| Openness | %d | %s | Height | %s |\n" val-open trait-open height))
    (insert (format "| Conscientiousness | %d | %s | Size | %s |\n" val-conc trait-conc size-desc))
    (insert (format "| Extroverted | %d | %s | Eye color | %s |\n" val-extr trait-extr eyes))
    (insert (format "| Agreeableness | %d | %s | Skin color | %s |\n" val-agre trait-agre skin))
    (insert (format "| Neurotic | %d | %s | Hair | %s %s %s |\n" val-neur trait-neur hair-len hair-col hair-sty))
    (insert (format "| | | | Facial hair | %s |\n" facial))
    (insert (format "| | | | Special features | %s |\n" features))
    (insert "|---+---+---+---+---|\n")
    (insert (format "| Chest | %s | | | |\n" chest))
    (insert (format "| Waist | %s | | | |\n" waist))
    (insert (format "| Bottom | %s | | | |\n" bottom))
    (insert (format "\n**Agenda:** %s %s, but %s.\n" goal focus obstacle))
    (previous-line 2) (org-table-align) (next-line 2)))

(provide 'enfors-ttrpg-npc)
