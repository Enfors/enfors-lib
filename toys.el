;;;; enfors-lib/toys.el

(defun enfors-draw-maze ()
  "Draw a maze-like pattern in the current buffer."
  (interactive)
  (random t)				; Seed the random function
  (dotimes (line 16)			; Draw 16 lines...
    (dotimes (column 60)		;   ... each consisting of 60 columns
      (if (eq 1 (random 2))		;     If (random 2) equals 1:
	  (insert "/")			;       then, insert "/"
	(insert "\\")))			;       else, insert (escaped) "\"
    (insert "\n")))			;     End of line, insert newline.

(defun enfors-practice-math (max)
  "Practice multiplication."
  (interactive "nEnter max: ")
  (message "Max is %d." max)
  (let ((num-questions 4)
	(a 0)
	(b 0)
	(answer 0)
	(correct-answers 0))
    (switch-to-buffer (generate-new-buffer "*enfors-practice-math*"))
    (dotimes (iter num-questions)
      (setq a (+ 2 (random (- max 1)))
	    b (+ 2 (random (- max 1))))
      (insert (format "Question %2d: %2d * %2d = " (+ 1 iter) a b))
      (setq answer (read-number "Enter answer: "))
      (insert (format "%d - " answer))
      (if (eq answer (* a b))
	  (progn
	    (insert "correct.\n")
	    (setq correct-answers (+ 1 correct-answers)))
	(insert (format "no, the answer is %d.\n"
			(* a b)))))
    (insert (format "You got %d out of %d correct.\n"
		    correct-answers num-questions))))

(defun enfors-dice (num max mod)
  "Roll a number of dice with a modifier. Insert results into current buffer."
  (interactive (list (read-number "Number of dice: ")
                     (read-number "Die size: ")
                     (read-number "Modifier: ")))
  (let ((index 0)
        (result 0)
        (operator "+"))
    (if (< mod 0)
        (setq operator "-"))
    (while (< index num)
      (setq result (+ result (random max) 1))
      (setq index (1+ index)))
    (if (eq mod 0)
        (insert (format "%dd%d: *%d*" num max result))
      (insert (format "%dd%d%s%d: %d %s %d = *%d*"
                      num max operator (abs mod) result operator (abs mod)
                      (+ result mod))))))

