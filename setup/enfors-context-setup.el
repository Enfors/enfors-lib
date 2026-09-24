;;; enfors-context-setup.el --- Setting agenda files -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; The purpose of this system is to be able to change "contexts" for Org
;; todos and agendas, etc.
;;
;; Next step: Add a transient menu for setting the context.
;;
;;; Code:
;;; Requires:

(require 'transient)

;;; Variables:



;; "Leaf" agenda file variables

(setq enfors-work-agenda-files
      '("~/devel/RoamNotes/20240920133750-lansforsakringar.org"
        "~/devel/RoamNotes/20260502113133-personal_todos.org"
        "~/devel/RoamNotes/20260131184817-calendar.org"
        "~/devel/RoamNotes/20260124205807-habits.org"
        "~/devel/RoamNotes/20220801134335-miramis.org"
        "~/devel/RoamNotes/20220527134741-tingvalla.org"
        "~/devel/RoamNotes/20260124144908-inbox.org"
        "~/devel/RoamNotes/20250121113929-unionen.org"
        "~/devel/RoamNotes/20220831105115-afry_todos.org")
      enfors-org-dev-agenda-files
      '("~/devel/RoamNotes/20260428085706-org_mode_contributor_liaison.org")
      enfors-karlstad-agenda-files
      '("")
      enfors-ekshärad-agenda-files
      '("20260411131443-eksharad.org")
      enfors-pf-campaign-agenda-files
      '("20240808163532-springhaven_pathfinder_campaign.org")
      enfors-ttrpg-hangout-agenda-files
      '("20240422141314-ttrpg_hangout.org"))

;; "Composite" agenda file variables
(setq enfors-home-agenda-files
      (append
       enfors-org-dev-agenda-files
       enfors-pf-capaign-agenda-files
       enfors-ttrpg-hangout-agenda-files))

(defvar enfors-contexts
  `(("Work" .                           ; The first context becomes the default
     ((key        . "w")
      (agenda-files  . ,enfors-work-agenda-files)))
    ("OrgDev" .
     ((key        . "o")
      (agenda-files  . ,enfors-org-dev-agenda-files)))))

(defvar enfors-context-name (car (car enfors-contexts)))


;;; Transient menu:

;; Define the menu (the prefix)

(transient-define-prefix enfors-context-menu ()
  "Enfors Context menu."
  ["Static Actions"
   ("q" "Quit" transient-quit-one)]
  
  ["Dynamic Contexts"
   :setup-children
   (lambda (_)
     ;; Parse our generated lists into actual Transient suffix objects.
     ;; The first argument must be the name of this prefix command.
     (transient-parse-suffixes
      'enfors-context-menu
      (mapcar (lambda (item)
                (let* ((name (car item))
                       (key  (alist-get 'key (cdr item))))
                  ;; Return a standard Transient suffix list
                  (list key
                        (format "Switch to %s" name)
                        (lambda ()
                          (interactive)
                          (enfors-context-set name)
                          (message "Context set to: %s" name)))))
              enfors-contexts)))])

(global-set-key (kbd "C-c C") 'enfors-context-menu)
;;; Support functions:

(defun enfors-context-get ()
  "Return the current context."
  (assoc enfors-context-name enfors-contexts))

(defun enfors-context-set (context-name)
  "Set the context corresponding to CONTEXT-NAME as the current context."
  (interactive "sContext: ")
  (let ((new-context (assoc context-name enfors-contexts)))
    (unless new-context
      (error (format "No such context: '%s'" context-name)))
    (message (format "Setting context to %s." context-name))
    (enfors--context-activate context-name)))

(defun enfors--context-activate (context-name)
  "Activate the context associated with the name CONTEXT-NAME."
  (let* ((context              (assoc context-name enfors-contexts))
         (context-agenda-files (cdr (assoc 'agenda-files context))))
    ;; Set agenda files
    (setq org-agenda-files context-agenda-files)
    (setq enfors-context-name context-name)))

(provide 'enfors-context-setup)
;;; enfors-context-setup.el ends here
