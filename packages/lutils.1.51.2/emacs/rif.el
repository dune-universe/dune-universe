
;;; rif.el  -  Major mode for rif files in Emacs
;;
;; Very very basic: provides only colors for keywords

;;;; to put in your .emacs:
; (autoload 'rif-mode 
;   "rif" "Major mode for editing rif file." t)

(require 'font-lock)


; version of rif-mode
(defconst rif-mode-version "0.3")


(defvar rif-mode-map nil
  "Keymap for rif major mode.")  
 

;;; Font-lock -----------------------------------------------------

(defvar rif-font-lock-keywords nil
  "Regular expression used by Font-lock mode.")

(setq rif-font-lock-keywords
      '(("--.*$" . font-lock-comment-face)
	
("\\<\\(inputs\\|step\\|locs\\|outs\\|inputs\\|outputs\\)\\>" . font-lock-string-face)
        ("[][#]\\.?\\|\\.\\."  . font-lock-function-name-face)
        ("\\<\\(true\\|T\\|F\\|f\\|t\\|false\\)\\>" . font-lock-reference-face)
	("\\<\\(bool\\|int\\|real\\|float\\)\\(\\^.+\\)?\\>" .  font-lock-variable-name-face)

	("\\(\\<\\(xxx\\|yyy\\)\\>\\|->\\)" . 
font-lock-function-name-face)))



(defun rif-font-mode ()
  "Initialisation of font-lock for Rif mode."
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(rif-font-lock-keywords t)))

; font-lock isn't used if in a  console
(if window-system
    (prog2
	(add-hook 'rif-mode-hook
		  'turn-on-font-lock)
	(add-hook 'rif-mode-hook
		  'rif-font-mode)))


;;; Major-mode

(add-to-list 'auto-mode-alist '("\\.rif$" . rif-mode))


(defun rif-mode ()
  " emacs mode for rif programs "

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rif-mode)
  (setq mode-name "Rif")
  (use-local-map rif-mode-map)  
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'electric-lustre-tab)
  (run-hooks 'rif-mode-hook)
)


(setq comment-start "#")


(provide 'rif)

;;; rif  .el ends here... 
