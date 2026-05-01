;;; enfors-gnus-setup.el -- my Gnus configuration
;;; Commentary:
;;; Code:
(require 'gnus)
;; (setq gnus-select-method            '(nntp "news.gmane.io"))
(setq gnus-select-method '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port "imaps")
               (nnimap-stream ssl)))

;; 2. Tell Emacs to use Gmail for sending (SMTP)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls)

;; 3. Enforce the classic 72-character line limit for outgoing mail
(add-hook 'message-mode-hook
          (lambda ()
            (setq fill-column 72)
            (auto-fill-mode 1)))

(setq gnus-secondary-select-methods
      '((nntp "news.gmane.io")
        (nntp "news.eternal-september.org"
              (nntp-port-number 563)
              (nntp-open-connection-function nntp-open-ssl-stream))))

;; Below is a verified working version, but probably doesn't use encryption
;; (setq gnus-secondary-select-methods
;;       '((nntp "news.gmane.io")
;;         (nntp "news.eternal-september.org"
;;               (nntp-port-number 119)
;;               (nntp-open-connection-function nntp-open-network-stream))))

;; (nntp-authinfo-file "~/.authinfo.gpg"))))
;; Recommended groups: comp.lang.c, comp.unix.shell


;; "For a minimal setup for posting should also customize the variables user-full-name and user-mail-address."
(setq user-full-name    "Christer Enfors"
      user-mail-address "christer.enfors@gmail.com")

;; Prevent gnus from generating its own MessageIDs for news
;(setq message-required-news-headers
;      (remove 'Message-ID message-required-news-headers))

(provide 'enfors-gnus-setup)

;;; enfors-gnus-setup.el ends here
