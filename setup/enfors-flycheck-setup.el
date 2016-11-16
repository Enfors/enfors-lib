;;; Package --- summary

;;; Commentary:

;;; Code:
(require 'package)
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(require 'flycheck)
(global-flycheck-mode)
(provide 'enfors-flycheck-setup)
;;; enfors-flycheck-setup.el ends here
