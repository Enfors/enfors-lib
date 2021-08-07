;;;; enfors-yasnippet-setup.el by Christer Enfors

(require 'yasnippet)
(yas-global-mode 1)
(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))
(provide 'enfors-yasnippet-setup)
