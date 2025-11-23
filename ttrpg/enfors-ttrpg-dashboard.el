;;; enfors-ttrpg-dashboard.el --- Tools for Solo RPGs  -*- lexical-binding: t; -*-

(require 'org)
(require 'seq)
(require 'hydra)

;; Load all sub-modules
(require 'enfors-ttrpg-core)
(require 'enfors-ttrpg-plot)
(require 'enfors-ttrpg-oracle)
(require 'enfors-ttrpg-npc)
(require 'enfors-ttrpg-names) ;; New Module

;; =============================================================================
;; DICE UI WRAPPERS
;; =============================================================================

(defun enfors-ttrpg-dice-roll-message ()
  "Roll dice and display the result in the minibuffer."
  (interactive)
  (let* ((input     (enfors-ttrpg-dice--get-input))
         (dice-str  (nth 0 input))
         (results   (enfors-ttrpg-dice--calculate dice-str))
         (raw-sum   (nth 0 results))
         (mod       (nth 1 results))
         (total     (nth 2 results)))
    (message "Rolled %s: %d %s %d = %d"
             dice-str raw-sum (if (>= mod 0) "+" "-") (abs mod) total)))

(defun enfors-ttrpg-dice-roll-insert ()
  "Roll dice and insert the result into the buffer."
  (interactive)
  (let* ((input     (enfors-ttrpg-dice--get-input))
         (dice-str  (nth 0 input))
         (ins-point (nth 1 input))
         (results   (enfors-ttrpg-dice--calculate dice-str))
         (total     (nth 2 results))
         (len       (length dice-str))
         (text-before (if (>= (- ins-point (point-min)) len)
                          (buffer-substring-no-properties (- ins-point len) ins-point)
                        ""))
         (already-in-buffer (string-equal text-before dice-str)))
    (goto-char ins-point)
    (if already-in-buffer
        (insert (format ": %d" total))
      (insert (format "%s: %d" dice-str total)))))

;; =============================================================================
;; DASHBOARD (HYDRA & STAGING)
;; =============================================================================

(defvar enfors-ttrpg-dashboard-origin-buffer nil
  "Stores the buffer where the dashboard was invoked, for 'Keep' actions.")

(defvar enfors-ttrpg-dashboard-window-config nil
  "Stores the window configuration before the dashboard was invoked.")

(defun enfors-ttrpg-stage-npc ()
  "Generate NPC into a staging buffer."
  (interactive)
  
  ;; 1. Save State (if not already in staging mode)
  (unless (get-buffer-window "*TTRPG-Stage*")
    (setq enfors-ttrpg-dashboard-window-config (current-window-configuration)))
    
  (unless (string= (buffer-name) "*TTRPG-Stage*")
    (setq enfors-ttrpg-dashboard-origin-buffer (current-buffer)))

  ;; 2. Generate
  (let ((buf (get-buffer-create "*TTRPG-Stage*")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode) 
      (enfors-ttrpg-npc-generate)) ;; Calls the function from enfors-ttrpg-npc.el
    (pop-to-buffer buf)))

(defun enfors-ttrpg-keep-stage ()
  "Move content from stage to origin and close stage."
  (interactive)
  (let ((content (with-current-buffer "*TTRPG-Stage*" (buffer-string))))
    ;; Paste into origin
    (with-current-buffer enfors-ttrpg-dashboard-origin-buffer
      (insert content))
    ;; Cleanup
    (kill-buffer "*TTRPG-Stage*")
    
    ;; Restore Layout
    (if enfors-ttrpg-dashboard-window-config
        (set-window-configuration enfors-ttrpg-dashboard-window-config)
      (switch-to-buffer enfors-ttrpg-dashboard-origin-buffer))
      
    (message "Content Saved.")))

(defun enfors-ttrpg-discard-stage ()
  "Close the stage without saving."
  (interactive)
  (when (get-buffer "*TTRPG-Stage*")
    (kill-buffer "*TTRPG-Stage*"))
  
  ;; Restore Layout
  (if enfors-ttrpg-dashboard-window-config
      (set-window-configuration enfors-ttrpg-dashboard-window-config)
    (when (buffer-live-p enfors-ttrpg-dashboard-origin-buffer)
      (switch-to-buffer enfors-ttrpg-dashboard-origin-buffer)))
      
  (message "Discarded."))

;; The Dashboard Menu
(defhydra enfors-ttrpg-dashboard (:color blue :hint nil)
  "
  ^Generators^             ^Oracles^                ^Dice^
  ^^^^^^^^-------------------------------------------------------
  _n_: NPC (Stage)         _y_: Yes/No (Ask)        _r_: Roll (Msg)
  _p_: Plot Generator      _o_: Action/Theme        _i_: Roll (Insert)
  _m_: Name (Male)
  _f_: Name (Female)
  
  ^Staging^
  ^^^^^^^^----------------
  _k_: Keep
  _d_: Discard
  _q_: Quit
  "
  ("n" enfors-ttrpg-stage-npc :color red)
  ("o" enfors-ttrpg-oracle-roll-action-theme :color red)
  ("y" enfors-ttrpg-oracle-ask :color red)
  ("p" enfors-ttrpg-plot-generate :color red)
  ("m" enfors-ttrpg-names-get-male :color red)
  ("f" enfors-ttrpg-names-get-female :color red)
  ("k" enfors-ttrpg-keep-stage)
  ("d" enfors-ttrpg-discard-stage)
  ("r" enfors-ttrpg-dice-roll-message :color red)
  ("i" enfors-ttrpg-dice-roll-insert :color red)
  ("q" nil))

;; BINDING (Active upon load)
(global-set-key (kbd "<f6>") 'enfors-ttrpg-dashboard/body)

(provide 'enfors-ttrpg-dashboard)
