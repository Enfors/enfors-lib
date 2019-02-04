;;; enfors-elpy-setup --- Configure elpy

;;; Commentary:

;; Elpy documentation: https://elpy.readthedocs.io/

;; Some useful hotkeys
;; ===================
;; M-.      : Goto definition
;; C-c C-z  : Start interactive Python shell
;; C-c C-e  : Globally rename identifyer under point
;;
;; Inside code completion dropdown menu thingie
;; ============================================
;; C-d      : Show documentation for current menu item

;;; Code:

(require 'package)
(add-to-list 'package-archives
	     '("elpy" . "http://jorgenschaefer.github.io/packages/"))
;; Now, elpy can be installed with M-x package-install RET elpy RET

;; Some features might need to be enabled with M-x elpy-config RET

;; Enable elpy by default
(package-initialize)
(elpy-enable)

;; Strip trailing whitespace before saving
(add-hook 'elpy-mode-hook
          (lambda () (add-to-list 'write-file-functions
                                  'delete-trailing-whitespace)))

;; Use Python 3, not Python 2
(setq elpy-rpc-python-command "python3")
(setq python-shell-interpreter "python3")

;; Try to make it use sensible colors
;(setq frame-background-mode "dark")

(provide 'enfors-elpy-setup)
;;; enfors-elpy-setup.el ends here
