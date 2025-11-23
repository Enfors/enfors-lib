;;; enfors-ttrpg-oracle.el --- Oracle Generators  -*- lexical-binding: t; -*-

(require 'enfors-ttrpg-core)

;; =============================================================================
;; CONFIGURATION
;; =============================================================================

(defcustom enfors-ttrpg-oracle-file "~/devel/RoamNotes/20250320221815-action_theme_oracle.org"
  "Path to the Org file containing Action/Theme tables."
  :type 'file
  :group 'enfors-ttrpg)

(defcustom enfors-ttrpg-oracle-yesno-file "~/devel/RoamNotes/20250320215503-yes_no_oracle.org"
  "Path to the Org file containing Yes/No tables."
  :type 'file
  :group 'enfors-ttrpg)

;; =============================================================================
;; HELPERS
;; =============================================================================

(defun enfors-ttrpg-oracle--parse-range (val)
  "Convert VAL (string '3-5' or number 2) to (min max).
Handles explicit integers from Org tables."
  ;; 1. If it's already a number, return (num num)
  (if (numberp val)
      (list val val)
    ;; 2. Otherwise ensure it's a string and parse
    (let ((str (format "%s" val)))
      (if (string-match "\\([0-9]+\\)-\\([0-9]+\\)" str)
          (list (string-to-number (match-string 1 str))
                (string-to-number (match-string 2 str)))
        (let ((num (string-to-number str)))
          (list num num))))))

(defun enfors-ttrpg-oracle--check-range (val cell-data)
  "Return t if VAL is within the range defined by CELL-DATA."
  (let* ((range (enfors-ttrpg-oracle--parse-range cell-data))
         (min (nth 0 range))
         (max (nth 1 range)))
    (and (>= val min) (<= val max))))

;; =============================================================================
;; ACTION / THEME ORACLE
;; =============================================================================

(defun enfors-ttrpg-oracle-roll-action-theme ()
  "Roll on the Action and Theme tables and display the result."
  (interactive)
  (let* ((action-res (enfors-ttrpg-lookup-multicolumn enfors-ttrpg-oracle-file "oracle-action"))
         (theme-res  (enfors-ttrpg-lookup-multicolumn enfors-ttrpg-oracle-file "oracle-theme")))
    (message "🔮 Oracle: %s / %s" 
             (nth 1 action-res) 
             (nth 1 theme-res))))

;; =============================================================================
;; YES / NO ORACLE
;; =============================================================================

(defun enfors-ttrpg-oracle-ask ()
  "Query the Yes/No oracle with probability selection."
  (interactive)
  (let* ((table (enfors-ttrpg-get-table enfors-ttrpg-oracle-yesno-file "oracle-yes-no"))
         ;; Format candidates as "[+Mod] Description" for easy reading
         (candidates (mapcar (lambda (row)
                               (let ((mod (nth 0 row))
                                     (desc (nth 1 row)))
                                 (cons (format "[%s] %s" mod desc) row)))
                             table))
         ;; Auto-find the "0" row to use as default
         (default-cand (car (seq-find (lambda (cand) (string-equal (format "%s" (nth 0 (cdr cand))) "0")) 
                                      candidates)))
         ;; Ask user to pick odds
         (selection (completing-read (format "Probability (default %s): " default-cand) 
                                     candidates nil t nil nil default-cand))
         (row (cdr (assoc selection candidates)))
         (roll (1+ (random 20)))
         (result nil))

    ;; Logic Tree
    (cond
     ;; 1. Negative Random Event (Natural 1)
     ((= roll 1)
      (let ((event-focus (enfors-ttrpg-roll-random-row enfors-ttrpg-oracle-yesno-file "oracle-random-event" 1)))
        (setq result (format "⚠️ NEGATIVE EVENT! (Focus: %s)" event-focus))))
     
     ;; 2. Positive Random Event (Natural 20)
     ((= roll 20)
      (let ((event-focus (enfors-ttrpg-roll-random-row enfors-ttrpg-oracle-yesno-file "oracle-random-event" 1)))
        (setq result (format "🌟 POSITIVE EVENT! (Focus: %s)" event-focus))))
     
     ;; 3. Standard Result
     (t 
      ;; Iterate through columns 2-7 (Indices 2 to 7 in the list)
      ;; Column headers: NoAnd(2), No(3), NoBut(4), YesBut(5), Yes(6), YesAnd(7)
      (let ((headers '("No, and" "No" "No, but" "Yes, but" "Yes" "Yes, and")))
        (dotimes (i 6)
          (let ((col-idx (+ 2 i))
                (header (nth i headers)))
            (when (enfors-ttrpg-oracle--check-range roll (nth col-idx row))
              (setq result header)))))))

    (message "🎲 Roll: %d %s -> %s" roll selection result)))

(provide 'enfors-ttrpg-oracle)
