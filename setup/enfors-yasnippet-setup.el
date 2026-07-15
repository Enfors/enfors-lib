;;; enfors-yasnippet-setup.el --- Yasnippet setup
;;; Commentary:
;;; Code:
(require 'yasnippet)
(yas-global-mode 1)
(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))
(provide 'enfors-yasnippet-setup)
;;; enfors-yasnippet-setup.el ends here
