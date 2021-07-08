
;;; lutin.el  -  Major mode for editing lustre source in Emacs


(require 'font-lock)


; version of lutin-mode
(defconst lutin-mode-version "0.0")


;;; Hooks

(defvar lutin-mode-hook nil
  "functions called when entering Lutin Mode.")

;;; Key-map for Lutin-mode

(defvar lutin-mode-map nil
  "Keymap for lutin major mode.")  


;;; Font-lock -----------------------------------------------------

(defvar lutin-font-lock-keywords nil
  "Regular expression used by Font-lock mode.")

(setq lutin-font-lock-keywords
      '(
        ("--.*$" . font-lock-comment-face)
        ("\\<\\(loop\\|fby\\||raise\\|try\\|trap\\|catch\\|do\\)\\>" . font-lock-builtin-face)	
        ("\\<\\(const\\|extern\\|node\\|run\\|include\\|returns\\|type\\)\\>" . font-lock-keyword-face)
        ("\\<\\(let\\|assert\\|exist\\|in\\|if\\|then\\|else\\)\\>" . font-lock-keyword-face)
        ("\\<\\(true\\|and\\|or\\|not\\|false\\)\\>" . font-lock-reference-face)
        ("\\<\\(trace\\|bool\\|int\\|ref\\|real\\)\\(\\^.+\\)?\\>" . font-lock-type-face)
        ("\\<\\(pre\\)\\>" . font-lock-constant-face)
))



(defun lutin-font-mode ()
  "Initialisation of font-lock for Lutin mode."
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(lutin-font-lock-keywords t)))

; font-lock isn't used if in a  console
(if window-system
    (prog2
	(add-hook 'lutin-mode-hook
		  'turn-on-font-lock)
	(add-hook 'lutin-mode-hook
		  'lutin-font-mode)))

   
(defun lutin-line-is-comment (&optional arg)
  "non-nil means line is only a commentary."
  (interactive)
  (save-excursion
    (beginning-of-line arg)
    (skip-chars-forward " \t")
    (looking-at "--")))


(setq comment-start "(*")
(setq comment-end "*)")


(setq comment-start "-- ")
(setq comment-end "")

;;; Major-mode

(defun lutin-mode ()
  "Major mode for editing Lutin files.

  Only keywords colaraition for the moment...
"

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lutin-mode)
  (setq mode-name "Lutin")
  (use-local-map lutin-mode-map)
  (run-hooks 'lutin-mode-hook))




(provide 'lutin)

;;; lutin .el ends here... 
