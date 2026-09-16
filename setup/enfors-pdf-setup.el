;;; enfors-pdf-setup --- My PDF configuration
;;; Commentary:
;;; Code:

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; Initialize the package; 't' prevents it from propting you
  ;; for confirming if the epdfinfo server ever needs to recompile
  (pdf-tools-install t)

  ;; Optional: Sets the default zoom level to fit the page on screen
  (setq-default pdf-view-display-size 'fit-page)

  :hook
  ;; Enable live preview updates
  (pdf-view-mode . auto-revert-mode))

;;; --------------------------------------------------------
;;; Smart Links & Missing Targets
;;; --------------------------------------------------------

(defun enfors/org-latex-smart-links (orig-fun link desc info)
  "Handle missing targets, phantom .org links, and auto-convert WebP images."
  (let ((type (org-element-property :type link)))
    (cond
     ;; 1. Internal links (CUSTOM_ID, fuzzy headings, Org-roam IDs)
     ((member type '("custom-id" "fuzzy" "id"))
      (let* ((target (ignore-errors 
                      (if (string= type "fuzzy")
                          (org-export-resolve-fuzzy-link link info)
                        (org-export-resolve-id-link link info))))
             (valid-target-p (and target
                                  (memq (org-element-type target)
                                        '(headline target radio-target table src-block item)))))
        (if valid-target-p
            (let ((latex-link (funcall orig-fun link desc info)))
              (if (string-match "\\\\hyperref\\[\\([^]]+\\)\\]" latex-link)
                  (format "%s (on page \\pageref*{%s})" 
                          latex-link 
                          (match-string 1 latex-link))
                latex-link))
          (or desc (org-element-property :raw-link link)))))

     ;; 2. File links (.org and .webp)
     ((string= type "file")
      (let* ((path (org-element-property :path link))
             (full-path (expand-file-name path)))
        (cond
         ;; Phantom .org links -> downgrade to text
         ((string-match-p "\\.org\\'" path)
          (or desc path))
         
         ;; WebP images -> auto-convert to PNG and rewrite AST path
         ((string-match-p "\\.webp\\'" path)
          (let ((png-path (concat (file-name-sans-extension full-path) ".png")))
            ;; Convert using ImageMagick if needed
            (when (or (not (file-exists-p png-path))
                      (file-newer-than-file-p full-path png-path))
              (call-process "convert" nil nil nil full-path png-path))
            
            ;; Temporarily swap the AST path to .png so LaTeX native exporter handles it perfectly
            (org-element-put-property link :path png-path)
            (let ((latex-output (funcall orig-fun link desc info)))
              ;; Put the original path back just to be safe
              (org-element-put-property link :path path)
              latex-output)))
         
         ;; All other standard files
         (t (funcall orig-fun link desc info)))))

     ;; 3. Everything else
     (t
      (funcall orig-fun link desc info)))))

(advice-add 'org-latex-link :around #'enfors/org-latex-smart-links)

;;; --------------------------------------------------------
;;; Warning Filters
;;; --------------------------------------------------------

(defun enfors/silence-pass1-latex-warnings (orig-fun buffer)
  "Remove Pass 1 undefined reference noise from the log buffer before Org reads it."
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          ;; Erase any line that contains the Pass 1 warning pattern
          (while (re-search-forward "^.*LaTeX Warning:.*Reference.*undefined.*$" nil t)
            (delete-region (line-beginning-position) (1+ (line-end-position))))))))
  ;; Now let Org-mode read the cleaned-up buffer
  (funcall orig-fun buffer))

(advice-add 'org-latex--collect-warnings :around #'enfors/silence-pass1-latex-warnings)
;;; --------------------------------------------------------
;;; Drawer & ID Handling
;;; --------------------------------------------------------

(defun enfors/hide-floating-properties (orig-fun drawer contents info)
  "Hide floating PROPERTIES drawers while keeping the LaTeX anchor."
  (if (string= (org-element-property :drawer-name drawer) "PROPERTIES")
      ;; If it's a properties drawer, generate the invisible anchor and discard the text!
      (format "\\phantomsection\n\\label{%s}\n" (org-export-get-reference drawer info))
    ;; If it's any other kind of drawer, print it normally
    (funcall orig-fun drawer contents info)))

(advice-add 'org-latex-drawer :around #'enfors/hide-floating-properties)

;;; --------------------------------------------------------
;;; WebP Image Handling & Path Fixing
;;; --------------------------------------------------------

;; 1. The Path Fixer (Forces Org to see the absolute Roam path, ONLY for PDF)
(defun enfors/fix-roam-image-paths (backend)
  "Convert relative image paths to absolute Roam paths during LaTeX export."
  (when (eq backend 'latex)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\(file:\\)?\\(\\./images/[^]]+\\)\\]\\]" nil t)
        (let* ((relative-path (match-string 2))
               (absolute-path (expand-file-name relative-path "~/devel/RoamNotes/")))
          (replace-match (format "[[file:%s]]" absolute-path)))))))

(add-hook 'org-export-before-parsing-hook #'enfors/fix-roam-image-paths)

;; 2. The WebP Rule (Tells Org that WebP is actually an image)
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-inline-image-rules 
               '("file" . "\\.\\(pdf\\|jpeg\\|jpg\\|png\\|ps\\|eps\\|tikz\\|pgf\\|svg\\|webp\\)\\'")))

;; 3. The Converter (Intercepts the LaTeX write, converts the file, and swaps the extension)

(provide 'enfors-pdf-setup)
;;; enfors-pdf-setup.el ends here
