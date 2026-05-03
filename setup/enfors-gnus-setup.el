;;; enfors-gnus-setup.el -- my Gnus configuration
;;; Commentary:
;;; Code:
(require 'gnus)

(setq gnus-select-method '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port "imaps")
               (nnimap-stream ssl)))

;; Tell Emacs to use Gmail for sending (SMTP)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls
      gnus-use-full-window nil       ; Respect my authoritah and C-x 3
      gnus-use-adaptive-scoring t)

;; Email sorting
;; 1 - Sort flat articles (and children in thread) by date, oldest first
(setq gnus-article-sort-functions
      '(gnus-article-sort-by-number
        gnus-article-sort-by-date))

;; 2 - Sort threads by the date of their root article, oldest first
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-date))

;; Turn off all the annoying HTML colors

;; 1. The "Prefer Plain Text" Rule
;; Most modern emails are sent as "multipart/alternative" (they
;; contain both text and HTML). This tells Gnus to actively despise
;; the HTML part and exclusively load the plain text.
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;; 2. Tame the HTML Renderer (For HTML-only emails)
;; If an email is *only* HTML, Gnus has no choice but to render it.
;; These settings strip out all the CSS colors, fonts, and images,
;; forcing the HTML to look exactly like standard, boring Emacs text.
(setq shr-use-colors nil)
(setq shr-use-fonts nil)
(setq shr-inhibit-images t)

;; Optional: If an HTML email has annoying clickable link markers,
;; this tones them down.
(setq shr-bullet "• ")

;; Posting styles
(setq gnus-posting-styles
      '((".*"  ; The default
         (eval (visual-line-mode 1))
         (eval (auto-fill-mode -1)))
        ("OrgList" ; The override for old style formatting
         (signature (concat "Christer Enfors\n"
                            "Org contributor liaison\n"
                            "GPG Fingerprint: 9CF7 3292 83BC D643 DF01  "
                            "992C 1C3B 434F 2AC2 B1E6\n"
                            "Public Key available at: keys.openpgp.org"))
         (eval (visual-line-mode -1))
         (eval (auto-fill-mode 1))
         (fill-column 72))))

;; This version (which I no longer want) forces all email lines to be
;; maximum 72 characters long.
;; (add-hook 'message-mode-hook
;;           (lambda ()
;;             (setq fill-column 72)
;;             (auto-fill-mode 1)))

(setq gnus-secondary-select-methods
      '((nntp "news.gmane.io")
        (nntp "news.eternal-september.org"
              (nntp-port-number 563)
              (nntp-open-connection-function nntp-open-ssl-stream))))

;; "For a minimal setup for posting should also customize the
;; variables user-full-name and user-mail-address."
(setq user-full-name    "Christer Enfors"
      user-mail-address "christer.enfors@gmail.com")

;; Prevent gnus from generating its own MessageIDs for news
;(setq message-required-news-headers
;      (remove 'Message-ID message-required-news-headers))

;;; Big Brother Database - for email address completion

;; ---------------------------------------------------------
;; THE DATABASE: BBDB (Big Brother Database)
;; ---------------------------------------------------------
(use-package bbdb
  :ensure t
  :custom
  ;; 'query = Ask me before saving. Change to t to save silently.
  (bbdb-mua-auto-update-p t)
  ;; Prevent BBDB from messing up window splits
  (bbdb-mua-pop-up nil)
  ;; Prevent TAB completion from opening the contact card window
  (bbdb-completion-display-record nil)
  :config
  ;; Hook BBDB into Gnus and Message-mode on load
  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'gnus 'message))

;; ---------------------------------------------------------
;; THE INTERFACE: Helm-BBDB
;; ---------------------------------------------------------
(use-package helm-bbdb
  :ensure t
  :after bbdb
  :bind (:map message-mode-map
              ;; Overwrite standard address lookup with Helm
              ("C-c b" . helm-bbdb)))

(provide 'enfors-gnus-setup)

;;; enfors-gnus-setup.el ends here
