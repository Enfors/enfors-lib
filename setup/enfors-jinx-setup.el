;;; enfors-jinx-setup.el --- configure jinx spell checker
;;; Commentary:
;;; External requirements:
;; sudo apt install libenchant-2-dev hunspell-en-us pkg-config build-essential
;;; Code:

(use-package jinx
  :ensure t
  :bind (("M-$" . jinx-correct))
  :hook ((text-mode . jinx-mode))
  :custom
  (jinx-exclude-regexps '((t "[A-ZÅÄÖ]\\w*" ; Ignore capitalized words
                             "\\w*[0-9]\\w" ; Ignore words with digits
                             ))))

(add-to-list 'helm-completing-read-handlers-alist '(jinx-correct . nil))

(provide 'enfors-jinx-setup)
;;; enfors-jinx-setup.el ends here
