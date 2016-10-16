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






      









