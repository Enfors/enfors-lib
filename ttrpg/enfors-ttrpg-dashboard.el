;;; enfors-ttrpg-dashboard.el --- Tools for Solo RPGs  -*- lexical-binding: t; -*-

(require 'org)
(require 'seq)
(require 'hydra)

;; Since this file and 'enfors-ttrpg-plot.el' are now in the same directory
;; (and that directory is added to load-path in your init.el), 
;; we can just require it directly.
(require 'enfors-ttrpg-plot)

;; =============================================================================
;; CONFIGURATION
;; =============================================================================

(defcustom enfors-ttrpg-npc-file "~/devel/RoamNotes/20250109111213-npc_generator.org"
  "Path to the Org-roam file containing NPC tables."
  :type 'file
  :group 'enfors-ttrpg)

(defcustom enfors-ttrpg-oracle-file "~/devel/RoamNotes/20250320221815-action_theme_oracle.org"
  "Path to the Org file containing Oracle tables."
  :type 'file
  :group 'enfors-ttrpg)

;; =============================================================================
;; GENERIC RPG ENGINE (The Bridge)
;; =============================================================================

(defun enfors-ttrpg-get-table (file table-name)
  "Retrieve data from a named Org table in FILE.
Removes horizontal lines ('hline) and drops the header row."
  ;; SAFETY CHECK: Verify file exists before trying to open it
  (unless (file-exists-p file)
    (error "TTRPG Error: Could not find file '%s'. Please check your configuration variables." file))

  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min)) ;; Reset point to ensure search starts from top
      (let* ((raw-data (org-babel-ref-resolve table-name)))
        (unless raw-data
          (error "TTRPG Error: Found file '%s', but could not find table named '%s'. Check your #+NAME: tags." file table-name))
        ;; Filter out separator lines
        (let ((clean-rows (seq-filter #'listp raw-data)))
          ;; Drop the header row (first element)
          (cdr clean-rows))))))

(defun enfors-ttrpg-roll-random-row (file table-name &optional col-idx)
  "Pick a random row from TABLE-NAME in FILE. Return content of COL-IDX (default 1)."
  (let* ((data (enfors-ttrpg-get-table file table-name))
         (row  (nth (random (length data)) data)))
    (nth (or col-idx 1) row)))

(defun enfors-ttrpg-roll-d8-reroll-8 ()
  "Roll 1d8, rerolling any 8s. Returns 1-7."
  (let ((val 8))
    (while (= val 8)
      (setq val (1+ (random 8))))
    val))

(defun enfors-ttrpg-lookup-multicolumn (file table-name)
  "Roll 1d100 and lookup the result in a multi-column table (5 pairs of 20 rows).
Returns a list: (roll text)."
  (let* ((data (enfors-ttrpg-get-table file table-name))
         (roll (1+ (random 100)))
         ;; LOGIC:
         ;; The table has 20 rows.
         ;; Roll 1-20 -> Row 0-19, Col Pair 0 (Index 1)
         ;; Roll 21-40 -> Row 0-19, Col Pair 1 (Index 3)
         ;; ...
         
         ;; 1. Determine which row (0-19)
         (row-idx (% (1- roll) 20))
         
         ;; 2. Determine which data column to pick
         ;; (Roll-1)/20 gives us the 'pair index' (0 to 4)
         ;; We multiply by 2 (to skip the numbers) and add 1 (to hit the text)
         (col-idx (1+ (* (/ (1- roll) 20) 2)))
         
         ;; 3. Fetch Data
         (row-data (nth row-idx data))
         (result-text (nth col-idx row-data)))
    
    (list roll result-text)))

;; =============================================================================
;; DICE ENGINE
;; =============================================================================

(defun enfors-ttrpg-dice--parse (dice-string)
  "Parse a string like '2d6+3' into a list: (count faces modifier)."
  (let ((regex "\\([0-9]*\\)d\\([0-9]+\\)\\(?:\\([+-][0-9]+\\)\\)?"))
    (if (string-match regex dice-string)
        (let* ((count-str (match-string 1 dice-string))
               (faces-str (match-string 2 dice-string))
               (mod-str   (match-string 3 dice-string)))
          (list
           (if (string-empty-p count-str) 1 (string-to-number count-str))
           (string-to-number faces-str)
           (if mod-str (string-to-number mod-str) 0)))
      (error "Invalid dice string: %s" dice-string))))

(defun enfors-ttrpg-dice--roll-sum (count faces)
  "Roll COUNT dice with FACES sides and return the sum."
  (let ((sum 0))
    (dotimes (_ count)
      (setq sum (+ sum 1 (random faces))))
    sum))

(defun enfors-ttrpg-dice--get-input ()
  "Return a list: (dice-string insertion-point).
If a valid dice string is at point, return it and its end position.
Otherwise, query the user."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if (and bounds
             (string-match-p "^[0-9]*d[0-9]+\\([+-][0-9]+\\)?$"
                             (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (list (buffer-substring-no-properties (car bounds) (cdr bounds))
              (cdr bounds))
      (list (read-string "Dice (e.g. 2d6+3): ")
            (point)))))

(defun enfors-ttrpg-dice--calculate (dice-string)
  "Calculate result for DICE-STRING. Returns list: (raw-sum modifier total)."
  (let* ((params (enfors-ttrpg-dice--parse dice-string))
         (count  (nth 0 params))
         (faces  (nth 1 params))
         (mod    (nth 2 params))
         (raw-sum (enfors-ttrpg-dice--roll-sum count faces))
         (total   (+ raw-sum mod)))
    (list raw-sum mod total)))

(defun enfors-ttrpg-dice-roll-message ()
  "Roll dice and display the result in the minibuffer."
  (interactive)
  (let* ((input     (enfors-ttrpg-dice--get-input))
         (dice-str  (nth 0 input))
         (results   (enfors-ttrpg-dice--calculate dice-str))
         (raw-sum   (nth 0 results))
         (mod       (nth 1 results))
         (total     (nth 2 results)))
    (message "Rolled %s: %d %s %d = %d"
             dice-str raw-sum (if (>= mod 0) "+" "-") (abs mod) total)))

(defun enfors-ttrpg-dice-roll-insert ()
  "Roll dice and insert the result into the buffer."
  (interactive)
  (let* ((input     (enfors-ttrpg-dice--get-input))
         (dice-str  (nth 0 input))
         (ins-point (nth 1 input))
         (results   (enfors-ttrpg-dice--calculate dice-str))
         (total     (nth 2 results))
         (len       (length dice-str))
         (text-before (if (>= (- ins-point (point-min)) len)
                          (buffer-substring-no-properties (- ins-point len) ins-point)
                        ""))
         (already-in-buffer (string-equal text-before dice-str)))
    (goto-char ins-point)
    (if already-in-buffer
        (insert (format ": %d" total))
      (insert (format "%s: %d" dice-str total)))))

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
;; ORACLE SPECIFIC LOGIC
;; =============================================================================

(defun enfors-ttrpg-oracle-roll ()
  "Roll on the Action and Theme tables and display the result."
  (interactive)
  (let* ((action-res (enfors-ttrpg-lookup-multicolumn enfors-ttrpg-oracle-file "oracle-action"))
         (theme-res  (enfors-ttrpg-lookup-multicolumn enfors-ttrpg-oracle-file "oracle-theme")))
    (message "🔮 Oracle: %s / %s  (Rolls: %d, %d)" 
             (nth 1 action-res) 
             (nth 1 theme-res) 
             (nth 0 action-res) 
             (nth 0 theme-res))))

;; =============================================================================
;; MAIN GENERATOR COMMANDS (NPC)
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

;; =============================================================================
;; DASHBOARD (HYDRA & STAGING)
;; =============================================================================

(defvar enfors-ttrpg-dashboard-origin-buffer nil
  "Stores the buffer where the dashboard was invoked, for 'Keep' actions.")

(defvar enfors-ttrpg-dashboard-window-config nil
  "Stores the window configuration before the dashboard was invoked.")

(defun enfors-ttrpg-stage-npc ()
  "Generate NPC into a staging buffer."
  (interactive)
  
  (unless (get-buffer-window "*TTRPG-Stage*")
    (setq enfors-ttrpg-dashboard-window-config (current-window-configuration)))
    
  (unless (string= (buffer-name) "*TTRPG-Stage*")
    (setq enfors-ttrpg-dashboard-origin-buffer (current-buffer)))

  (let ((buf (get-buffer-create "*TTRPG-Stage*")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode) 
      (enfors-ttrpg-npc-generate))
    (pop-to-buffer buf)))

(defun enfors-ttrpg-keep-stage ()
  "Move content from stage to origin and close stage."
  (interactive)
  (let ((content (with-current-buffer "*TTRPG-Stage*" (buffer-string))))
    (with-current-buffer enfors-ttrpg-dashboard-origin-buffer
      (insert content))
    (kill-buffer "*TTRPG-Stage*")
    
    (if enfors-ttrpg-dashboard-window-config
        (set-window-configuration enfors-ttrpg-dashboard-window-config)
      (switch-to-buffer enfors-ttrpg-dashboard-origin-buffer))
      
    (message "NPC Saved.")))

(defun enfors-ttrpg-discard-stage ()
  "Close the stage without saving."
  (interactive)
  (when (get-buffer "*TTRPG-Stage*")
    (kill-buffer "*TTRPG-Stage*"))
  
  (if enfors-ttrpg-dashboard-window-config
      (set-window-configuration enfors-ttrpg-dashboard-window-config)
    (when (buffer-live-p enfors-ttrpg-dashboard-origin-buffer)
      (switch-to-buffer enfors-ttrpg-dashboard-origin-buffer)))
      
  (message "NPC Discarded."))

;; The Dashboard Menu
(defhydra enfors-ttrpg-dashboard (:color blue :hint nil)
  "
  ^Generators^             ^Dice^
  ^^^^^^^^-----------------------------------
  _n_: NPC (Stage)         _r_: Roll (Msg)
  _o_: Oracle (Act/Thm)    _i_: Roll (Insert)
  _p_: Plot Generator
  
  ^Staging^
  ^^^^^^^^----------------
  _k_: Keep
  _d_: Discard
  _q_: Quit
  "
  ("n" enfors-ttrpg-stage-npc :color red)
  ("o" enfors-ttrpg-oracle-roll :color red)
  ("p" enfors-ttrpg-plot-generate :color red)
  ("k" enfors-ttrpg-keep-stage)
  ("d" enfors-ttrpg-discard-stage)
  ("r" enfors-ttrpg-dice-roll-message :color red)
  ("i" enfors-ttrpg-dice-roll-insert :color red)
  ("q" nil))

;; BINDING (Active upon load)
(global-set-key (kbd "<f6>") 'enfors-ttrpg-dashboard/body)

(provide 'enfors-ttrpg-dashboard)
