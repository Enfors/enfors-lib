;;; enfors-cipher-setup.el --- cipher functions
;;; Commentary:

;; enfors-cipher-text converts the text and the key to uppercase only, then
;; skips any characters that aren't a-z or A-Z, so that the output text will
;; only be a continuous string of zero or more uppercase letters, regardless of
;; what other characters may be included in the input string.

;;; Code:
(defun enfors-cipher-text (text key direction)
  "Return enciphered/deciphered TEXT using KEY based on DIRECTION (1 or -1)."
  (let ((text          (upcase text))
        (key           (upcase key))
        (text-pos      0)
        (key-pos       0)
        (key-len       (length key))
        (text-char     0)
        (text-char-val 0)
        (key-char      0)
        (key-char-val  0)
        (new-char-val  0)
        (output    ""))
    (when (< (length text) 1)
      (user-error "The text has to be longer than zero bytes"))
    (when (< key-len 1)
      (user-error "The key has to be longer than zero bytes"))
    ;; For each position in the source text
    (dotimes (text-pos (length text))
      ;; Set text-char and key-char to corresponding numerical values
      (setq text-char (aref text text-pos))
      (setq key-char  (aref key  key-pos))
      ;; Only do the following if it is a letter
      (when (and (>= text-char ?A)
                 (<= text-char ?Z))
        ;; Get the ASCII values from text-char and key-char, subtracting 64
        ;; ?A == 65
        (setq text-char-val (- text-char ?A -1)
              key-char-val  (- key-char  ?A -1))
        ;; This is where the enciphering or deciphering happens, depending on
        ;; if direction is 1 (enciphering) or -1 (deciphering).
        (setq new-char-val (+ text-char-val
                              (* key-char-val direction)
                              ?A -1))
        ;; If the new "character" is "higher" than Z, scale it down
        (when (> new-char-val ?Z)
          (setq new-char-val (- new-char-val 26)))
        ;; If the new "character" is "lower" than Z, scale it up
        (when (< new-char-val ?A)
          (setq new-char-val (+ new-char-val 26)))

        ;; Add the new character to the output
        (setq output (concat output (string new-char-val)))
        ;; Go to the next key position
        (setq key-pos (+ key-pos 1))
        ;; Check if we're past the end of the key, if so, scale it back.
        (setq key-pos (mod key-pos key-len))))
    output))

(defun enfors-encipher-region (start end key)
  "Insert the enciphered text from the region between START and END, using KEY."
  (interactive "r\nsCipher key: ")
  (let ((enciphered-text (enfors-cipher-text
                          (buffer-substring-no-properties start end)
                          key 1)))
    (insert "\n")
    (insert enciphered-text)
    (message "%d characters enciphered." (length enciphered-text))))

(defun enfors-decipher-region (start end key)
  "Message the deciphered text from the region between START and END, using KEY."
  (interactive "r\nsCipher key: ")
  (message
   (enfors-cipher-text (buffer-substring-no-properties start end) key -1)))

(provide 'enfors-cipher-setup)
;;; enfors-cipher-setup.el ends here
