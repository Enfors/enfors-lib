;;;; ERC
;;; Code:
(require 'erc)
(set-face-foreground 'erc-input-face "yellow")
;(set-face-foreground 'erc-nick-default-face "blue")

(setq erc-hide-list '("JOIN" "PART" "QUIT")
(setq erc-nick      "Enfors")

(add-hook 'erc-mode-hook (lambda () (setq scroll-step 1
                                          scroll-conservatively 10000)))

;;; The following section was copied from
;;; https://www.emacswiki.org/emacs/ErcNickColors, Option 2:
;; Pool of colors to use when coloring IRC nicks.
(setq erc-colors-list '("red" "lightred"
                        "blue" "lightblue"
                        "cyan" "darkcyan"
                        "green" "darkgreen"
                        "magenta" "lightmagenta" "darkmagenta"
                        "white"))
;; special colors for some people
(setq erc-nick-color-alist '(("John" . "blue")
			     ("Bob" . "red")
                             ("AzuraBot" . "blue")
			     ))

(defun erc-get-color-for-nick (nick)
  "Gets a color for NICK. If NICK is in erc-nick-color-alist, use that color, else hash the nick and use a random color from the pool"
  (or (cdr (assoc nick erc-nick-color-alist))
      (nth
       (mod (string-to-number
	     (substring (md5 (downcase nick)) 0 6) 16)
	    (length erc-colors-list))
       erc-colors-list)))

(defun erc-put-color-on-nick ()
  "Modifies the color of nicks according to erc-get-color-for-nick"
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "<\\([^>]*\\)>")
	(let ((nick (match-string 1)))
	  (put-text-property (match-beginning 1) (match-end 1) 'face
			     (cons 'foreground-color
				   (erc-get-color-for-nick nick)))))))

(add-hook 'erc-insert-modify-hook 'erc-put-color-on-nick)
;;; End of section copied from Emacswiki

(provide 'enfors-erc-setup)
;;; enfors-erc-setup ends here
