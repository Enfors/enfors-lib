;;;; enfors-yasnippet-setup.el by Christer Enfors

(if (file-accessible-directory-p "~/.emacs.d/plugins/yasnippet")
    (progn
      (add-to-list 'load-path
		   "~/.emacs.d/plugins/yasnippet")
      (require 'yasnippet)
      (yas-global-mode 1)
      (provide 'enfors-yasnippet-setup))
  (message "yasnippet doesn't appear to be installed."))