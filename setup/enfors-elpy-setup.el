;;; enfors-elpy-setup --- Configure elpy

;;; Commentary:

;;; Code:

(require 'package)
(add-to-list 'package-archives
	     '("elpy" . "http://jorgenschaefer.github.io/packages/"))
;; Now, elpy can be installed with M-x package-install RET elpy RET

;; Some features might need to be enabled with M-x elpy-config RET

;; Enable elpy by default
(package-initialize)
(elpy-enable)

;; Use Python 3, not Python 2
(setq elpy-rpc-python-command "python3")

(provide 'enfors-elpy-setup)
;;; enfors-elpy-setup.el ends here
