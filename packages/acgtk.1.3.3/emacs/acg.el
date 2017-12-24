;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        
;;                 ACG development toolkit                                
;;                                                                        
;;                  Copyright 2008 INRIA                                  
;;                                                                        
;;  More information on "http://acg.gforge.inria.fr/"                     
;;  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     
;;  Authors: see the AUTHORS file                                         
;;                                                                        
;;                                                                        
;;                                                                        
;;                                                                        
;;  $Rev::                              $:  Revision of last commit       
;;  $Author::                           $:  Author of last commit         
;;  $Date::                             $:  Date of last commit           
;;                                                                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Main missing features: indentation. Only a cheap implementation is
;; provided

(require 'generic-x)

(define-generic-mode
  'acg-mode
  (cons "(*" '("*)"))
  '("prefix"
    "infix"
    "binder"
    "end"
    "type"
    "signature"
    "lexicon"
    "nl_lexicon"
    "<<")
  '(
    ; FIXME while this regexp correctly capture multi-line comments,
    ; they they're not highlighted in the emace buffer. Only single
    ; line comments are
    ("\\((\\*\\([^*]*\\*[^)].*\\|[^*]*\\|.*[^*])\\|[^)]*\\)\\(\n\2\\)*\\*)\\)" 1 ''font-lock-comment-face)
    ("\\(signature\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \t\n]*\\(=\\)" 1 'font-lock-keyword-face)
    ("\\(signature\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 2 'font-lock-constant-face)
    ("\\(signature\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 3 'font-lock-keyword-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 1 'font-lock-keyword-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 2 'font-lock-constant-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 3 'font-lock-keyword-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 4 'font-lock-constant-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 5 'font-lock-keyword-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 6 'font-lock-keyword-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 7 'font-lock-constant-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 8 'font-lock-keyword-face)


    ("\\(nl_lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 1 'font-lock-keyword-face)
    ("\\(nl_lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 2 'font-lock-constant-face)
    ("\\(nl_lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 3 'font-lock-keyword-face)
    ("\\(nl_lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 4 'font-lock-constant-face)
    ("\\(nl_lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 5 'font-lock-keyword-face)
    ("\\(nl_lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 6 'font-lock-keyword-face)
    ("\\(nl_lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 7 'font-lock-constant-face)
    ("\\(nl_lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 8 'font-lock-keyword-face)

    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(<<\\)" 1 'font-lock-keyword-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(<<\\)" 2 'font-lock-constant-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(<<\\)" 3 'font-lock-keyword-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(<<\\)" 4 'font-lock-constant-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(<<\\)" 5 'font-lock-keyword-face)
    ("\\([a-zA-Z0-9_']*\\)[ \t\n]*\\(<<\\)[ \t\n]*\\([a-zA-Z0-9_']*\\)" 1 'font-lock-constant-face)
    ("\\([a-zA-Z0-9_']*\\)[ \t\n]*\\(<<\\)[ \t\n]*\\([a-zA-Z0-9_']*\\)" 2 'font-lock-keyword-face)
    ("\\([a-zA-Z0-9_']*\\)[ \t\n]*\\(<<\\)[ \t\n]*\\([a-zA-Z0-9_']*\\)" 3 'font-lock-constant-face)
        )
  '(".*\\.acg")
  nil
)

; regexp pour les commentaires
; (\*\([^*]*\*[^)].*\|[^*]*\|.*[^*])\|[^)]*\)\(\n\1\)*\*)


;;;;;;;;;;;;;;;;;;;;;;;
;; User customization
;;;;;;;;;;;;;;;;;;;;;;;

(require 'custom)

(defgroup acg nil
  "Support for the ACG definition language"
  :group 'languages)

(defcustom acg-default-indent 4  
  "*Default indentation.

Global indentation variable (large values may lead to indentation overflows).
When no governing keyword is found, this value is used to indent the line
if it has to."
  :group 'acg :type 'integer)



(defgroup acg-faces nil
  "Special faces for the Acg mode."
  :group 'acg)

(defconst acg-faces-inherit-p
  (if (boundp 'face-attribute-name-alist)
      (assq :inherit face-attribute-name-alist)))

(defface acg-font-lock-error-face
  (if acg-faces-inherit-p
      '((t :inherit font-lock-warning-face))
    '((t (:foreground "yellow" :background "red")))
    )
  "Face description for all errors reported to the source."
  :group 'acg-faces)
(defvar acg-font-lock-error-face
  'acg-font-lock-error-face)


;; Define the key map for the acg-mode
(setq acg-mode-map (make-sparse-keymap))
(define-key acg-mode-map "\C-c\C-c" 'compile)

(defun acg-set-mode-map ()
  "Set the ACG mode map."
  (interactive)
  (use-local-map acg-mode-map)
)
(add-hook 'acg-mode-hook 'acg-set-mode-map)

;; Define the compilation command
(defun acg-set-compile-command ()
  "Hook to set compile-command locally." 
  (interactive)
  (let* ((filename (file-name-nondirectory buffer-file-name))
	 (basename (file-name-sans-extension filename))
	 (command nil))
    (if (executable-find "acgc.opt")
	(setq command "acgc.opt")
      (if (executable-find "acgc")
	  (setq command "acgc")))
    (progn
      (make-local-variable 'compile-command)
      (setq compile-command (concat command " " filename))))
  )

(add-hook 'acg-mode-hook 'acg-set-compile-command)

(require 'compile)

;; find the line of the error
(defconst acg-error-regexp
;;  "^[^\0-@]+ \"\\([^\"\n]+\\)\", [^\0-@]+ \\([0-9]+\\)[-,:]"
  "^.*File \"\\([^\"]+\\)\",[^0-9]+\\([0-9]+\\),"
  "Regular expression matching the error messages produced by acgc.")

(if (boundp 'compilation-error-regexp-alist)
    (or (assoc acg-error-regexp
               compilation-error-regexp-alist)
        (setq compilation-error-regexp-alist
              (cons (list acg-error-regexp 1 2)
               compilation-error-regexp-alist))))

;; A regexp to extract the range info.
;; Needs to be augmented with the possible optional range info
;; (for instance in case of non linear application on linear variable
(defconst acg-error-chars-single-line-regexp
;;  ".*, .*, [^\0-@]+ \\([0-9]+\\)-\\([0-9]+\\)"
  ".*line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\)"
  "Regexp matching the char numbers in an error message produced by acgc.")


(defconst acg-error-chars-multi-line-regexp
  ".*line \\([0-9]+\\), character \\([0-9]+\\) to line \\([0-9]+\\), character \\([0-9]+\\)"
  "Regexp matching the char numbers in an error message produced by acgc.")


(defalias 'acg-match-string
  (if (fboundp 'match-string-no-properties)
      'match-string-no-properties
    'match-string))

(defadvice next-error (after acg-next-error activate)
 "Read the extra positional information provided by the Acg compiler.

Puts the point and the mark exactly around the erroneous program
fragment. The erroneous fragment is also temporarily highlighted if
possible."
 (if (eq major-mode 'acg-mode)
     (let ((beg nil) (end nil) (line-beg nil) (line-end nil) (char-end nil))
       (save-excursion
	 (set-buffer compilation-last-buffer)
	 (save-excursion
	   (goto-char (window-point (get-buffer-window (current-buffer) t)))
	   (if (looking-at acg-error-chars-single-line-regexp)
	       (setq line-beg (string-to-int (acg-match-string 1))
		     beg (string-to-int (acg-match-string 2))
		     end (string-to-int (acg-match-string 3)))
	     (if (looking-at acg-error-chars-multi-line-regexp)
	       (setq line-beg (string-to-int (acg-match-string 1))
		     beg (string-to-int (acg-match-string 2))
		     line-end (string-to-int (acg-match-string 3))
		     char-end (string-to-int (acg-match-string 4))
		     )))))
;       (goto-line line-beg)
       (beginning-of-line)
       (if beg
	   (progn
	     (if end
		 (progn
		   (setq beg (+ (point) beg) end (+ (point) end))
		   (goto-char beg)
		   (push-mark (+ end 1) t t))
	       (progn
		 (setq beg (+ (point) beg))
		 (goto-char beg)
		 (setq current-position nil)
		 (point-to-register current-position)
		 (goto-line line-end)
		 (beginning-of-line)
		 (setq end (+ (point) char-end))
		 (push-mark (+ end 1) t t)
		 (jump-to-register current-position))
	       )
	     )
	 )
       )
   )
 )

(ad-activate 'next-error)


; This code provides a very cheap indentation procedure.  It basically
; expects the signature, lexicon and end kwd to be the first
; declaration on the line and that for each declaration (or
; definition) not to have anything else after the closing ";"

; To allow the algorithm work with long files.
(setq max-lisp-eval-depth 10000)


;(make-local-variable 'indent-line-function)
(setq indent-line-function 'acg-indent-line)

(defun acg-find-position-from-first-non-blank-char ()
;  (message "point is: %s" (current-column))
;  (message "indentation is: %s" (current-indentation))
  (if (> (current-column) (current-indentation))
      (- (current-column) (current-indentation))
    0))

(defun acg-move-backward-line-skipping-empty-lines ()
;  (message "Moving backward from %d" (line-number-at-pos))
  (if (forward-line -1)
      (progn 
	(while (and (not (looking-at "^[ \t]*[^ \t\n].*$")) (not (bobp)))
	  (forward-line -1))
	t)
    (nil)))

(defun acg-check-current-line-belongs-to-a-comment ()
  (let (in-comment))
;  (message "Checking line: %d" (line-number-at-pos))
  (save-excursion
					; assume we're in a comment by
					; default
    (if (acg-move-backward-line-skipping-empty-lines)
	(cond 
	 ((looking-at "^[ \t]*(\\*.*\\*)[ \t]*$") ; the line is a one line comment
	  (progn
;	    (message "The previous line is a one line comment")
	    (setq in-comment nil)))
	 ((looking-at "^.*\\*)[ \t]*$") ; the line ends closing a comment
	  (progn
;	    (message "The previous line closes a comment")
	    (setq in-comment nil)))
	 ((looking-at "^[ \t]*(\\*") ; the line opens a comment without closing it
	  (progn
;	    (message "The previous line opens a comment")
	    (setq in-comment t)))
	 ((bobp)
	  (progn
;	    (message "Reaching the beginning of the buffer")
	    (setq in-comment nil)))
	 (t ; there is no mention of a comment
	  (progn
					;	    (message "Don't know. I need to move backward")
	    (setq in-comment (acg-check-current-line-belongs-to-a-comment)))))
      (setq in-comment nil)
      )
    )
  in-comment)

(defun acg-indent-line ()
  "Indent current line as ACG declaration"
  (interactive "*")
  (setq offset-from-non-blank (acg-find-position-from-first-non-blank-char))
  (setq inside-comment-tab 3)
;  (message "the offset is %s" offset-from-non-blank)
;  (message "Starting acg-indent-line at line: %d" (line-number-at-pos))
  (beginning-of-line)
;  (message "set to: %d after beginning-of-line" (line-number-at-pos))
  (setq cur-indent nil)
  (if (acg-check-current-line-belongs-to-a-comment )
      (progn
;	(message "current line is in a comment")
	(save-excursion
	  (acg-move-backward-line-skipping-empty-lines)
	  (while (and (acg-check-current-line-belongs-to-a-comment) (not (bobp)))
	    (acg-move-backward-line-skipping-empty-lines))
	  (setq cur-indent (+ inside-comment-tab (current-indentation)))))
     (progn
;       (message "current line is %d and is not in a comment" (line-number-at-pos))
       (if (bobp)
					; if beginning of buffer, set
					; indentation to 0
	 (setq cur-indent 0)
					; otherwise first look if
					; there is some keyword
					; starting the line
       (if (looking-at "^[ \t]*\\(lexicon\\|signature\\|end\\)")
					; and also set indentation to 0
	   (progn
	     (setq cur-indent 0)
					;(message "I found a kwd so cur-indent is set to 0")
	     )
	 ; we're in a declaration or a definition
	 ; let's try to see whether the previous one was closed
	 (progn
	   (save-excursion
	     (acg-move-backward-line-skipping-empty-lines)
	     (if (acg-check-current-line-belongs-to-a-comment)
		 (setq cur-indent (max 0 (- (current-indentation) inside-comment-tab)))
	       (progn
		 (cond
		  ((looking-at "^[ \t]*\\(lexicon\\|signature\\)")
		   (setq cur-indent  acg-default-indent)
		   )
		  ((looking-at "^[ \t]*end")
		   (setq cur-indent 0)
		   )
		  ((looking-at "^[ \t]*(\\*.*\\*)[ \t]*$")
		   (setq cur-indent (current-indentation))
		   )
		  ((looking-at "^.*;[ \t]*$")
		   (setq cur-indent  acg-default-indent)
		   )
		  ((looking-at "^.*,[ \t]*$")
		   (if (= (current-indentation) acg-default-indent)
		       (setq cur-indent  (+ (current-indentation) acg-default-indent))
		     (setq cur-indent  (current-indentation))
		     )
		   )
		  ((looking-at "^.*\\(->\\|\.\\)[ \t]*$")
		   (setq cur-indent  (+ (current-indentation) acg-default-indent))
		   )
		  (t
		   (setq cur-indent (+ acg-default-indent acg-default-indent))
		   )
		  ))))))))
     )
  (if cur-indent
      (indent-line-to cur-indent)
    (indent-line-to 0))
  (forward-char offset-from-non-blank)
  )	      
      



