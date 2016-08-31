(defun ce-ins-day (week-day)
  "Insert a day plan at point."
  (interactive "sEnter day of week: ")
  
  (insert "
**** ")
  (insert week-day)
  (insert "
	  
| Uppgift       | Pomodoros |
|---------------+-----------|
|               |           |
| Avsluta dagen | ()        |

"))


(defun ce-ins-week (week-num)
  "Insert a week plan at point."
  (interactive "sEnter week number: ")
  (insert "
*** Vecka ")
  (insert week-num)
  (insert "

| Dag     | Pomodoros |
|---------+-----------|
| Måndag  |           |
| Tisdag  |           |
| Onsdag  |           |
| Torsdag |           |
| Fredag  |           |
| Totalt  |           |

")
  (ce-ins-day "Måndag")
  (ce-ins-day "Tisdag")
  (ce-ins-day "Onsdag")
  (ce-ins-day "Torsdag")
  (ce-ins-day "Fredag")
  (forward-line -35)
  (forward-char 2))

(provide 'enfors-day-plan-setup)

