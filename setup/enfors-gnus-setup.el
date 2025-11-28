;;;; Gnus
;;; Code:
(require 'gnus)
(setq gnus-select-method            '(nntp "news.gmane.io"))

(setq gnus-secondary-select-methods
      '((nntp "news.eternal-september.org"
              (nntp-port-number 119)
              (nntp-open-connection-function nntp-open-network-stream)
              (nntp-stream-type starttls))))

;                                            (nntp-authinfo-file "~/.authinfo.gpg"))))
;; Recommended groups: comp.lang.c, comp.unix.shell


;; "For a minimal setup for posting should also customize the variables user-full-name and user-mail-address."
(setq user-full-name    "Christer Enfors"
      user-mail-address "christer.enfors@gmail.com")

;; Prevent gnus from generating its own MessageIDs for news
(setq message-required-news-headers
      (remove 'Message-ID message-required-news-headers))

(provide 'enfors-gnus-setup)
