;;; enfors-yasnippet-setup.el --- Yasnippet setup
;;; Commentary:
;;; Code:
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" enfors-path)))
  :config
  (yas-global-mode 1)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

(provide 'enfors-yasnippet-setup)
;;; enfors-yasnippet-setup.el ends here
