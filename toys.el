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

