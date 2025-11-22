;;;; dice.el -- By Christer Enfors with help of Google Gemini 3

(defun enfors-dice--parse (dice-string)
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

(defun enfors-dice--roll-sum (count faces)
  "Roll COUNT dice with FACES sides and return the sum."
  (let ((sum 0))
    (dotimes (_ count)  ; Loop COUNT times. '_' is used for variables we don't need.
      ;; (random faces) returns 0 to faces-1, so we add 1
      (setq sum (+ sum 1 (random faces))))
    sum))

(defun enfors-dice--get-input ()
  "Return a list: (dice-string insertion-point).
If a valid dice string is at point, return it and its end position.
Otherwise, query the user and return the input and current point."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if (and bounds
             (string-match-p "^[0-9]*d[0-9]+\\([+-][0-9]+\\)?$"
                             (buffer-substring-no-properties (car bounds) (cdr bounds))))
        ;; Found valid dice at point. Return string and the end position (cdr bounds).
        (list (buffer-substring-no-properties (car bounds) (cdr bounds))
              (cdr bounds))
      ;; Nothing found. Ask user. Return input and current point.
      (list (read-string "Dice (e.g. 2d6+3): ")
            (point)))))

(defun enfors-dice--calculate (dice-string)
  "Calculate result for DICE-STRING. Returns list: (raw-sum modifier total)."
  (let* ((params (enfors-dice--parse dice-string))
         (count  (nth 0 params))
         (faces  (nth 1 params))
         (mod    (nth 2 params))
         (raw-sum (enfors-dice--roll-sum count faces))
         (total   (+ raw-sum mod)))
    (list raw-sum mod total)))

(defun enfors-dice-roll-message ()
  "Roll dice and display the result in the minibuffer."
  (interactive)
  (let* ((input     (enfors-dice--get-input))
         (dice-str  (nth 0 input))
         ;; We don't need the insertion point (nth 1) for the message version.
         (results   (enfors-dice--calculate dice-str))
         (raw-sum   (nth 0 results))
         (mod       (nth 1 results))
         (total     (nth 2 results)))

    ;; Display the result nicely
    (message "Rolled %s: %d %s %d = %d"
             dice-str
             raw-sum
             (if (>= mod 0) "+" "-")
             (abs mod)
             total)))

(defun enfors-dice-roll-insert ()
  "Roll dice and insert the result into the buffer."
  (interactive)
  (let* ((input     (enfors-dice--get-input))
         (dice-str  (nth 0 input))
         (ins-point (nth 1 input))
         (results   (enfors-dice--calculate dice-str))
         (total     (nth 2 results))
         ;; Logic to decide if we print "2d6: 7" or just ": 7"
         (len       (length dice-str))
         ;; Look backwards from insertion point to see if the dice string is already there
         (text-before (if (>= (- ins-point (point-min)) len)
                          (buffer-substring-no-properties (- ins-point len) ins-point)
                        ""))
         (already-in-buffer (string-equal text-before dice-str)))

    ;; Move cursor to the correct end position
    (goto-char ins-point)

    ;; Insert the result
    (if already-in-buffer
        (insert (format ": %d" total))      ; Just the result
      (insert (format "%s: %d" dice-str total))))) ; The label + result

(with-eval-after-load 'org
  ;; "i" for Insert (Log the roll into the buffer)
  (define-key org-mode-map (kbd "C-c d i") 'enfors-dice-roll-insert)

  ;; "m" for Message (Just tell me the result)(
  (define-key org-mode-map (kbd "C-c d m") 'enfors-dice-roll-message))
