;;; enfors-ttrpg-core.el --- Core functions for TTRPG tools  -*- lexical-binding: t; -*-

(require 'org)
(require 'seq)

;; =============================================================================
;; TABLE ENGINE
;; =============================================================================

(defun enfors-ttrpg-get-table (file table-name)
  "Retrieve data from a named Org table in FILE.
Removes horizontal lines ('hline) and drops the header row."
  (unless (file-exists-p file)
    (error "TTRPG Error: Could not find file '%s'." file))

  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (let* ((raw-data (org-babel-ref-resolve table-name)))
        (unless raw-data
          (error "TTRPG Error: Could not find table '%s' in '%s'." table-name file))
        (let ((clean-rows (seq-filter #'listp raw-data)))
          (cdr clean-rows))))))

(defun enfors-ttrpg-roll-random-row (file table-name &optional col-idx)
  "Pick a random row from TABLE-NAME in FILE. Return content of COL-IDX (default 1)."
  (let* ((data (enfors-ttrpg-get-table file table-name))
         (row  (nth (random (length data)) data)))
    (nth (or col-idx 1) row)))

(defun enfors-ttrpg-lookup-multicolumn (file table-name)
  "Roll 1d100 and lookup result in a multi-column table (5 pairs of 20 rows)."
  (let* ((data (enfors-ttrpg-get-table file table-name))
         (roll (1+ (random 100)))
         (row-idx (% (1- roll) 20))
         (col-idx (1+ (* (/ (1- roll) 20) 2)))
         (row-data (nth row-idx data))
         (result-text (nth col-idx row-data)))
    (list roll result-text)))

;; =============================================================================
;; DICE ENGINE
;; =============================================================================

(defun enfors-ttrpg-roll-d8-reroll-8 ()
  "Roll 1d8, rerolling any 8s. Returns 1-7."
  (let ((val 8))
    (while (= val 8)
      (setq val (1+ (random 8))))
    val))

(defun enfors-ttrpg-dice--parse (dice-string)
  "Parse '2d6+3' into (count faces modifier)."
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
  "Roll COUNT dice with FACES sides."
  (let ((sum 0))
    (dotimes (_ count)
      (setq sum (+ sum 1 (random faces))))
    sum))

(defun enfors-ttrpg-dice--calculate (dice-string)
  "Calculate result for DICE-STRING. Returns (raw-sum modifier total)."
  (let* ((params (enfors-ttrpg-dice--parse dice-string))
         (count  (nth 0 params))
         (faces  (nth 1 params))
         (mod    (nth 2 params))
         (raw-sum (enfors-ttrpg-dice--roll-sum count faces))
         (total   (+ raw-sum mod)))
    (list raw-sum mod total)))

(defun enfors-ttrpg-dice--get-input ()
  "Get dice string from point or query user."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if (and bounds
             (string-match-p "^[0-9]*d[0-9]+\\([+-][0-9]+\\)?$"
                             (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (list (buffer-substring-no-properties (car bounds) (cdr bounds))
              (cdr bounds))
      (list (read-string "Dice (e.g. 2d6+3): ")
            (point)))))

(provide 'enfors-ttrpg-core)
