;;; enfors-transclusion-setup.el

(use-package org-transclusion
  :ensure t
  :bind
  ("<f12>" . org-transclusion-mode)
  :config
  ;; Optional: Make the transcluded text look slightly different
  ;; (background color) so you know it's "projected" from another file.
  (set-face-attribute 'org-transclusion-fringe nil :foreground "green" :background "green"))

;; Force the Left Fringe to be 10 pixels wide (Right fringe 0)
(fringe-mode '(10 . 0))

;; Make the Transclusion Bar bright green
(set-face-attribute 'org-transclusion-fringe nil
                    :foreground "#22ff22"
                    :background "#22ff22")

(custom-set-faces
 '(org-transclusion-keyword
   ((t (:background "#005500" :foreground "#ffffff" :weight bold :box t)))))

(provide 'enfors-transclusion-setup)
