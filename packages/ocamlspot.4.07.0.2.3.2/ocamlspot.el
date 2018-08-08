; (***********************************************************************)
; (*                                                                     *)
; (*                            OCamlSpotter                             *)
; (*                                                                     *)
; (*                             Jun FURUSE                              *)
; (*                                                                     *)
; (*   Copyright 2008, 2009 Jun Furuse. All rights reserved.             *)
; (*   This file is distributed under the terms of the GNU Library       *)
; (*   General Public License, with the special exception on linking     *)
; (*   described in file LICENSE.                                        *)
; (*                                                                     *)
; (***********************************************************************)

; # How-to-use: Write the following to your .emacs
;
;     ; load-path
;      (setq load-path (cons "WHERE-YOU-HAVE-INSTALLED-THIS-ELISP" load-path))
;     
;      (require 'ocamlspot)
;     
;     ; tuareg mode hook (use caml-mode-hook instead if you use caml-mode)
;      (add-hook 'tuareg-mode-hook
;              '(lambda ()
;                 (local-set-key "\C-c;" 'ocamlspot-query)
;      	     (local-set-key "\C-c:" 'ocamlspot-query-interface)
;                 (local-set-key "\C-c'" 'ocamlspot-query-uses)
;                 (local-set-key "\C-c\C-t" 'ocamlspot-type)
;                 (local-set-key "\C-c\C-i" 'ocamlspot-xtype)
;                 (local-set-key "\C-c\C-y" 'ocamlspot-type-and-copy)
;                 (local-set-key "\C-ct" 'caml-types-show-type)
;                 (local-set-key "\C-cp" 'ocamlspot-pop-jump-stack)))
;     
;     ; set the path of the ocamlspot binary.
;      (setq ocamlspot-command "WHERE-YOU-HAVE-INSTALLED-THE-BINARIES/ocamlspot.opt")
;     
;     ; Optional: You can also change overlay colors as follows:
;     ;  (set-face-background 'ocamlspot-spot-face "#660000")
;     ;  (set-face-background 'ocamlspot-tree-face "#006600")
;
;
; # Setup
;
; M-x customize-group => ocamlspot
;
; # Commands
;
; ocamlspot-query
;   Show the type of the inner-most subexpression under the cursor.
;   If there is an identifier under the cursor, browse and show its definition
;
; ocamlspot-query-interface
;   Same as ocamlspot-query but browse identifier's interface rather than its defintion
;   This is currently under construction and does not work properly.
;
; ocamlspot-type
;   Show the type of the inner-most subexpression under the cursor.
;
; ocamlspot-type-and-copy
;   Same as ocamlspot-type but it also copies the type expression to the kill buffer.
;
; ocamlspot-use
;   Show the use information of the identifier under the cursor.
;
; ocamlspot-xtype
;   Same as ocamlspot-type but it shows you more detailed information: 
;   path id numbers. You can browse path names in this result using C-c;



; CR jfuruse: no tree node found is displayed when ocamlspot program is not found


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Configurable variables

(eval-when-compile (require 'cl)) ; for `destructuring-bind'

(defgroup ocamlspot ()
  "OCamlSpotter: find the definition and type of variables."
  :group 'languages)

(defcustom ocamlspot-command "ocamlspot"
  "*The command which invokes ocamlspot."
  :type 'string :group 'ocamlspot)

(defcustom ocamlspot-debug nil
  "*Turn on ocamlspot debug output."
  :type 'boolean :group 'ocamlspot)

(defcustom ocamlspot-samewindow nil
  "Use current window to show the spot."
  :type 'boolean :group 'ocamlspot)

(defcustom ocamlspot-use-cygpath nil
  "Perform necessary Windows/Cygwin path name conversions if you use Cygwin Emacs and MinGW/MSVC OCaml."
  :type 'boolean :group 'ocamlspot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constants

;; Buffer names

(defconst ocamlspot-process-buffer "*ocamlspot-process*"
  "The name of ocamlspot communication buffer")

(defconst ocamlspot-debug-buffer "*ocamlspot-debug*"
  "The name of ocamlspot debugging buffer")

(defconst ocamlspot-message-buffer "*ocamlspot-message*"
  "The name of ocamlspot message buffer")

(defconst ocamlspot-type-buffer "*ocamlspot-type*"
  "The name of ocamlspot type buffer")

(defconst ocamlspot-cygpath-buffer "*ocamlspot-cygpath*"
  "The name of ocamlspot cygpath communication buffer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; column chars => column bytes

;; This looks complicated, but we need this conversion for multi-byte characters
;; OCaml's colmuns are in bytes, but Emacs's columns are in chars.

;; returns string from the beginning of the line at the point to the point.
(defun ocamlspot-string-of-line-to-point ()
  (buffer-substring-no-properties
   (line-beginning-position) (point)))

;; returns byte length from the beginning of the line at the point to the point.
(defun ocamlspot-bytes-of-line-to-point ()
  (length
   (encode-coding-string
    (ocamlspot-string-of-line-to-point) buffer-file-coding-system)))

;; It count one line less when the cursor is at (point-max) 
;; and it is at the top of the line.
(defun ocamlspot-lines-of-point ()
  (count-lines (point-min) (min (1+ (point)) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; column bytes => column chars

;; This looks complicated, but we need this conversion for multi-byte characters

;; Same as (goto-line), but without unwanted side-effects.
(defun ocamlspot-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

;; returns the string at line
(defun ocamlspot-buffer-substring-at-line (line)
  ; no need of save-excursion
  (ocamlspot-goto-line line)
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

;; returns the first [bytes] of [str] as chars, not as bytes
(defun ocamlspot-chars-of-bytes-of-string (str bytes)
  (length
   (decode-coding-string
    (substring (encode-coding-string str buffer-file-coding-system)
               0 bytes)
    buffer-file-coding-system)))

;; returns the buffer position of [bytes] at [line]
(defun ocamlspot-pos-of-bytes-at-line (line bytes)
  ; no need of save-excursion
  (ocamlspot-goto-line line)
  (let ((pos-at-beginning-of-line (line-beginning-position))
        (chars-from-beginning-of-line
         (ocamlspot-chars-of-bytes-of-string
          (ocamlspot-buffer-substring-at-line line) bytes)))
    (+ pos-at-beginning-of-line chars-from-beginning-of-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; location parser

; parses lxxxcxxxbxxx and returns the triplet
(defun ocamlspot-parse-location (s)
  (if (string-match "^l\\([\-0-9]+\\)c\\([\-0-9]+\\)b\\([\-0-9]+\\)$" s)
      (let ((line (string-to-number (match-string 1 s)))
            (colbytes (string-to-number (match-string 2 s)))
            (bytes (string-to-number (match-string 3 s))))
        (list line colbytes bytes))))

;; convert lxxxxcxxxxbxxxx to its buffer position
(defun ocamlspot-pos-of-location (buffer s)
  (destructuring-bind (line colbytes bytes) (ocamlspot-parse-location s)
    (if (= line -1) bytes
      (with-current-buffer buffer
        (ocamlspot-pos-of-bytes-at-line line colbytes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Messaging

(defvar ocamlspot-message-file-name nil) ;; only used for xtype

;; Clean the message buffer, and set the context file name
(defun ocamlspot-message-init (context-file-name)
  (setq ocamlspot-message-file-name context-file-name)
  (with-current-buffer (get-buffer-create ocamlspot-message-buffer)
    (erase-buffer)))

;; Add a message to the message buffer
(defun ocamlspot-message-add (mes)
  (with-current-buffer (get-buffer-create ocamlspot-message-buffer)
    (if (/= 0 (current-column))
        (insert "\n"))
    (insert mes)))

;; Display message in the echo area if it is enough short, then return the string.
;; If too large, pop a buffer of the message if may-pop is t and return the buffer.
;; Otherwise, returns nil
(defun ocamlspot-message-display (&optional may-pop)
  (with-current-buffer (get-buffer-create ocamlspot-message-buffer)
    (let ((lines ; how many lines in minibuffer-window ? 
           (count-screen-lines nil nil nil (minibuffer-window)))
          (max-echo-height 
           (if resize-mini-windows
               (cond ((floatp max-mini-window-height)
                      (* (frame-height) max-mini-window-height))
                     ((integerp max-mini-window-height)
                      max-mini-window-height)
                     (t 1)))))

      (if (or (<= lines  1)
              (<= lines max-echo-height))
          (progn
            (let ((mes (buffer-string)))
              (message "%s" mes)
              mes))
        (if may-pop ; buffer layout may change... no way to recover ?
            (progn
              (display-buffer ocamlspot-message-buffer)
              ocamlspot-message-buffer)
          ;; display the first max-echo-height lines
          (let ((lines (max 1 (1- max-echo-height))))
            (goto-char (point-min))
            (forward-visible-line (max 1 (- max-echo-height 2)))
            (message "%s" (concat (buffer-substring (point-min) (point)) "... Result is too long. Truncated."))
            nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; type buffer

;; Clean the type buffer
(defun ocamlspot-type-init ()
  (with-current-buffer (get-buffer-create ocamlspot-type-buffer)
    (erase-buffer)
    (ocamlspot-xtype-mode t)))

;; Add message to the type buffer
(defun ocamlspot-type-add (mes)
  (with-current-buffer (get-buffer-create ocamlspot-type-buffer)
    (if (/= 0 (current-column))
        (insert "\n"))
    (insert mes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Overlays

;; the spot overlay
(defvar ocamlspot-spot-overlay (make-overlay 1 1))
(defface ocamlspot-spot-face
    '((t (:foreground "#88FF44")))
  "Face for ocamlspot spot highlight"
  :group 'ocamlspot)
(overlay-put ocamlspot-spot-overlay 'face 'ocamlspot-spot-face)

;; the tree overlay
(defvar ocamlspot-tree-overlay (make-overlay 1 1))
(defface ocamlspot-tree-face
    '((t (:foreground "#FF88FF")))
  "Face for ocamlspot tree highlight"
  :group 'ocamlspot)
(overlay-put ocamlspot-tree-overlay 'face 'ocamlspot-tree-face)

;; Clear the overlay 
(defun ocamlspot-delete-overlays-now ()
  (interactive)
  (delete-overlay ocamlspot-tree-overlay)
  (delete-overlay ocamlspot-spot-overlay))

;; Clear the overlay, waiting 10 secs maximum
(defun ocamlspot-delete-overlays ()
  (unwind-protect
      (sit-for 10)
    (ocamlspot-delete-overlays-now)))

;; Parse ocamlspot region string
;; Acceptable forms: all | lxxxcxxxbxxx:lyyycyyybyyy | xxx:yyy
(defun ocamlspot-convert-region (buffer position)
  (if (not buffer) (error "ocamlspot-convert-region: buffer is nill")
    (save-current-buffer
      (set-buffer buffer)
      (if (or (string-equal "all" position) (string-equal "-1:-1" position))
	  (list (point-min) (point-max))
	(if (string-match "^\\(l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|[0-9]+\\):\\(l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|[0-9]+\\)$" position)
	    (let ((start (match-string 1 position))
		  (end   (match-string 2 position)))
	      (let ((start (ocamlspot-pos-of-location buffer start))
		    (end   (ocamlspot-pos-of-location buffer end)))
		(list start end)))
	  nil)))))

(defun ocamlspot-display-overlay (buffer emacs-start-end overlay)
  (if emacs-start-end
      (progn
	(destructuring-bind (start end) emacs-start-end
	  ;; display the result
	  (set-buffer buffer)
	  (goto-char start)
	  (move-overlay overlay start end buffer)))
    (message "ocamlspot-display-overlay: strange region")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Warnings

;; Search a warning from the current point
(defun ocamlspot-warning ()
  (and (re-search-forward "^\\(Warning: .*\\)$" nil t)
       (match-string 1)))

;; Search the warnings from the current point, and returns them in reversed order
(defun ocamlspot-warnings-rev (lst)
  (let ((warning (ocamlspot-warning)))
    (if warning (ocamlspot-warnings-rev (concat lst warning "\n"))
      lst)))

;; Search the warnings from the current point
(defun ocamlspot-warnings ()
  (goto-char (point-min))
  (ocamlspot-warnings-rev ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Windows <=> Cygwin path conversion

(defun encode-path (path)
  (if ocamlspot-use-cygpath
      (progn
	(with-current-buffer (get-buffer-create ocamlspot-cygpath-buffer)
	  (erase-buffer)
	  (call-process "cygpath" nil ocamlspot-cygpath-buffer nil "-w" path)
	  (replace-regexp-in-string "\r?\n" "" (buffer-string))))
    path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; File access

;; Open the file, if exists
(defun ocamlspot-find-file-existing (path)
  (if (file-exists-p path)
      (if ocamlspot-samewindow
          (find-file path)
        (find-file-other-window path))
    (ocamlspot-message-add (format "ERROR: source file %s was not found" path))
    (error (format "ERROR: source file %s was not found" path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Queries

;; Run ocamlspot command with args, possibly in the given directory.
(defun ocamlspot-run-query (args &optional chdir)
  (with-current-buffer (get-buffer-create ocamlspot-process-buffer)
    (ocamlspot-process-mode t)
    (erase-buffer)
    (insert (mapconcat 'identity (cons ocamlspot-command args) " "))
    (insert "\n")
    ;; chdir is required
    (if chdir (cd chdir))
    (let ((args (if ocamlspot-debug (cons "--debug" args) args)))
      (print (append '(call-process ocamlspot-command nil t nil) args))
      (eval (append '(call-process ocamlspot-command nil t nil) args)))))

;; Creates the query location string of the point
(defun ocamlspot-query-string-at-cursor ()
  (format "%s:l%dc%d"
	  (encode-path (buffer-file-name))
	  (ocamlspot-lines-of-point)
	  (ocamlspot-bytes-of-line-to-point)))

;; launch ocamlspot, using the position of the cursor
;; result is stored in the buffer "ocamlspot-process-buffer"
;; the current buffer is stored in source-buffer
(defun ocamlspot-query-at-cursor (pre_extra_args &optional post_extra_args)
  ;; arguments
  (let ((file-name (buffer-file-name))
	(arg (ocamlspot-query-string-at-cursor)))
    (ocamlspot-run-query (append pre_extra_args (list arg) post_extra_args)
			 (file-name-directory file-name))))

;; Search ocamlspot-process-buffer from the top and return the first line which matches with ^<pattern>: "
;; If [to-kill], the output is copied to the kill-buffer.
(defun ocamlspot-find-query-result (pattern &optional to-kill)
  (save-current-buffer
    (set-buffer (get-buffer-create ocamlspot-process-buffer))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" pattern ": \\(.*\\(\n +.*\\)*\\)") nil t)
	;; We got \r here with Cygwin Emacs + MinGW/VS OCaml 
	(let ((the-match (replace-regexp-in-string "\r" "" (match-string 1))))
	  (if to-kill (kill-new the-match))
	     the-match))))

;; Scan the ocamlspot process output and search a Tree tag.
;; If there is a Tree result, highlight it and returns the position string
;; Otherwise returns nil.
(defun ocamlspot-find-tree ()
  (save-excursion
    (let ((source-buffer (current-buffer)))
      (with-current-buffer (get-buffer-create ocamlspot-process-buffer)
	;; search the found tree element
	(let ((tree (ocamlspot-find-query-result "Tree")))
	  (if tree 
	      (progn
		(let ((start-end (ocamlspot-convert-region source-buffer tree)))
		  (if start-end
		      (save-current-buffer
			(ocamlspot-display-overlay source-buffer start-end ocamlspot-tree-overlay)))
		  (let ((err (ocamlspot-find-query-result "Error")))
		    (if err
			(ocamlspot-message-add (concat "Error: " err))))
		  start-end))

	    (let ((err (ocamlspot-find-query-result "Error")))
	      (if err
		  (ocamlspot-message-add (concat "Error: " err))
		(ocamlspot-message-add "Error: no tree node found there")))

	    (let ((err (ocamlspot-find-query-result "Uncaught exception")))
	      (if err
		  (ocamlspot-message-add (concat "Error: ocamlspot raised an exception!!: " err))))
		
	    nil))))))

;; Jump to [position] of [filename], with highlighting the spot overlay
(defun ocamlspot-jump-to-spot (filename position)
  (if (string-match "\.cm[ioxa]$" filename)
      ;; It is not an .ml or .mli. Packed module.
      ;; CR jfuruse: opening a binary file is not good
      (ocamlspot-message-add (format "Packed module: %s" filename))
    (let* ((buffer (ocamlspot-find-file-existing filename))
	   (start-end (ocamlspot-convert-region buffer position)))
      (if start-end
	  (ocamlspot-display-overlay buffer start-end ocamlspot-spot-overlay)
	  (ocamlspot-message-add (concat "Error: strange position: " position))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Jump history

;; Experimental. Jump back to the latest query position.

(defvar ocamlspot-jump-stack nil)

(defun ocamlspot-pop-jump-stack ()
  (interactive)
  (if ocamlspot-jump-stack
      (progn
	(destructuring-bind (buffer pos) (car ocamlspot-jump-stack)
	  (setq ocamlspot-jump-stack (cdr ocamlspot-jump-stack))
	  (if (buffer-live-p buffer)
	      (progn
		(display-buffer buffer)
		(switch-to-buffer buffer)
		(goto-char pos))
	    (ocamlspot-pop-jump-stack))))
    (message "OCamlSpot jump stack is empty")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Find-and-do-somethings. Query must be already done.

;; Jump to given path-range.
(defun ocamlspot-jump-to-path-range (path-range)
  (if (string-match "^<?\\(.*\\):\\(all\\|[\-0-9lcb]+:[\-0-9lcb]+[^>]*\\)>?$" path-range)
      (let ((filename (match-string 1 path-range))
	    (position (match-string 2 path-range)))
	;; preserve current buffer and pos ;; CR jfuruse: history preserving should take occur only when the jump is successful.
	(setq ocamlspot-jump-stack (cons (list (current-buffer)
					       (point))
					 ocamlspot-jump-stack))

	;; display the result
	(ocamlspot-jump-to-spot filename position)
	(let ((type (ocamlspot-find-val-or-type)))
	  ;; (if type (ocamlspot-message-add (format "Type: %s" type)))
	  ))
    ;; failed to get the normal result
    ;; This can be: Spot: %a: predefined %s
    (ocamlspot-message-add path-range)))

;; Show the type information
;; If [to-kill], the output is copied to the kill-buffer.
(defun ocamlspot-find-type (&optional to-kill)
  (let ((type (ocamlspot-find-query-result "Type" to-kill)))
    (if type 
	(progn
	  (ocamlspot-message-add (format "Type: %s" type))
	  (ocamlspot-type-add (format "Type: %s" type))
	  type)
      (ocamlspot-message-add "No type found")
      nil)))

;; same as type-in-buffer but for XType
(defun ocamlspot-find-xtype ()
  (let ((type (ocamlspot-find-query-result "XType")))
    (if type
	(progn
	  (ocamlspot-message-add (format "(* %s *)\n" ocamlspot-message-file-name))
	  (ocamlspot-message-add (format "%s" type))
	  (ocamlspot-type-add (format "(* %s *)\n" ocamlspot-message-file-name))
	  (ocamlspot-type-add (format "%s" type))
	  type)
      (ocamlspot-message-add "No type found")
      nil)))

;; If [to-kill], the output is copied to the kill-buffer.
(defun ocamlspot-find-val-or-type (&optional to-kill)
  (let ((type (ocamlspot-find-query-result "Val" to-kill)))
    (if type
	(progn 
	  (ocamlspot-message-add (format "Val: %s" type))
	  (ocamlspot-type-add (format "Val: %s" type))
	  type)
      (ocamlspot-find-type to-kill))))

;; Show use info
(defun ocamlspot-find-use ()
  (let ((use (ocamlspot-find-query-result "Use")))
    (if use
	(progn 
	  (ocamlspot-message-add (format "Use: %s" use))
	  use)
      (ocamlspot-message-add "no use information found here")
      nil)))

;; Jump to the position found at the Spot tag
(defun ocamlspot-find-spot ()
  (let ((spot (ocamlspot-find-query-result "Spot")))
    (if spot (ocamlspot-jump-to-path-range spot)
      ;; no Spot:
      (let ((err (ocamlspot-find-query-result "Error")))
	(if err
	    (ocamlspot-message-add (concat "Error: " err))
	  ;; display debug info
	  (ocamlspot-message-add "No definition info found")
	  (ocamlspot-find-val-or-type)
	)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactives

;; Show the message, and wait at most 10secs, then clear the overlay highlights
(defun ocamlspot-wait (&optional may-pop)
  (ocamlspot-message-display may-pop)
  (ocamlspot-delete-overlays))

(defun ocamlspot-query (&rest args)
  (interactive)
  (let ((sel-window (selected-window)))
  (save-selected-window
    (ocamlspot-message-init (buffer-file-name))
    (ocamlspot-type-init)
    (ocamlspot-delete-overlays-now)
    (ocamlspot-query-at-cursor args)
    (when (ocamlspot-find-tree)
      ;; search the result
      (ocamlspot-find-spot))
    (ocamlspot-wait))
  ;; I dunno why but we need the following line to list-buffers work nicely
  (select-window sel-window)))

(defun ocamlspot-query-interface ()
  (interactive)
  (ocamlspot-query "--interface"))

;; Query the type at the point and show it. 
;; If [to-kill] is t, the type string is copied to the kill buffer.
(defun ocamlspot-type (&optional to-kill)
  (interactive)
  (ocamlspot-message-init (buffer-file-name))
  (ocamlspot-type-init)
  (ocamlspot-delete-overlays-now)
  (ocamlspot-query-at-cursor '("-n"))  
  (if (ocamlspot-find-tree)
      (save-current-buffer
        (ocamlspot-find-val-or-type to-kill)))
  (ocamlspot-wait t))

(defun ocamlspot-type-and-copy ()
  (interactive)
  (ocamlspot-type t))

(defun ocamlspot-xtype (&optional to-kill)
  (interactive)
  (ocamlspot-message-init (buffer-file-name))
  (ocamlspot-type-init)
  (ocamlspot-delete-overlays-now)
  (ocamlspot-query-at-cursor '("-n"))
  (if (ocamlspot-find-tree)
      (save-current-buffer
        (ocamlspot-find-xtype)))
  (display-buffer ocamlspot-type-buffer))

; CR can be shared with ocamlspot-type
(defun ocamlspot-use ()
  (interactive)
  (ocamlspot-message-init (buffer-file-name))
  (ocamlspot-type-init)
  (ocamlspot-delete-overlays-now)
  (ocamlspot-query-at-cursor '("-n"))
  (if (ocamlspot-find-tree)
      (save-current-buffer
        (ocamlspot-find-use)))
  (ocamlspot-wait t))

; CR can be shared with ocamlspot-type
(defun ocamlspot-query-uses ()
  (interactive)
  (let ((dir (read-directory-name "Search directory: "
				  (file-name-directory (buffer-file-name)))))
    (ocamlspot-message-init (buffer-file-name))
    (ocamlspot-type-init)
    (ocamlspot-delete-overlays-now)
    (ocamlspot-query-at-cursor (list "use" dir))
    (if (ocamlspot-find-tree)
	(progn
	 (ocamlspot-find-spot)
	 (display-buffer ocamlspot-process-buffer)
	 (ocamlspot-find-use)))
    (ocamlspot-wait t)))

; ;; expand expr/pattern by type
(defun ocamlspot-expand ()
  (interactive)
  (ocamlspot-message-init (buffer-file-name))
  (ocamlspot-type-init)
  (ocamlspot-query-at-cursor (list "-n" "--type-expand"))
  (let ((expansion (ocamlspot-find-query-result "Expand")))
    (if expansion
	(let ((start-end (ocamlspot-find-tree)))
	  (if start-end
	      (destructuring-bind (start end) start-end
                (ocamlspot-delete-overlays-now)
		(delete-region start end) ; or kill-region ?
		(goto-char start)
		(insert expansion)
		(let ((new-end (point)))
		  (ocamlspot-display-overlay (current-buffer) (list start new-end) ocamlspot-tree-overlay))
		(ocamlspot-delete-overlays)
		)
	    (ocamlspot-message-add "OCamlSpot -expand: no tree information found")))
      (ocamlspot-message-add "OCamlSpot -expand: no expand information found"))))
	
;; Browsing of path-range <file:lxcxbx:lxcxbx>
(defun ocamlspot-beginning-of-path-range ()
  (search-backward "<"
		   (save-excursion (beginning-of-line) (point))))
(defun ocamlspot-end-of-path-range ()
  (search-forward ">"
		  (save-excursion (end-of-line) (point))))
(put 'ocamlspot-path-range 'beginning-op 'ocamlspot-beginning-of-path-range)
(put 'ocamlspot-path-range 'end-op 'ocamlspot-end-of-path-range)

(defun ocamlspot-path-range-at-point ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'ocamlspot-path-range)))
      (if bounds
	  (progn
	    (move-overlay ocamlspot-tree-overlay (car bounds) (cdr bounds))
	    (buffer-substring (car bounds) (cdr bounds))))))

(defun ocamlspot-browse-path-range-at-point ()
  (interactive)
  (let ((path-range (ocamlspot-path-range-at-point)))
    (if path-range
	(ocamlspot-jump-to-path-range path-range)
	(message "no path-range at point")
	)))

;; build query string for the cursor point
(defun ocamlspot-xtype-build-query-at-cursor ()
  (let ((path-name 
	 (save-excursion
	   (let ((end
;; Preferable, but not working correctly yet for A.B<cursor>.t
;;		  (if (skip-chars-forward "A-z0-9_)")
;;		      (point)
;;		    nil))
		  (if (skip-chars-forward "A-z0-9_().") ;; less powerful
		      (point)
		    nil))
		 (start
		  (if (skip-chars-backward "A-z0-9_().")
		      (point)
		    nil)))
	     (if (and start end) 
		 (progn
		   (move-overlay ocamlspot-tree-overlay start end)
		   (buffer-substring-no-properties start end)))))))
    (message "%s" (concat "path-name " path-name))
    (let ((file-name 
	   (save-excursion
	     (goto-char (point-min))
	     (if (re-search-forward "^(\\* \\(.*\\) \\*)$" nil t)
		 (match-string 1)))))
      (if (and 
	   file-name 
	   path-name 
	   (not (string= file-name "")) 
	   (not (string= path-name "")))
	  (concat file-name ":t:" path-name)))))

(defun ocamlspot-xtype-query ()
  (interactive)
  (let ((sel-window (selected-window)))
    (save-selected-window
      (ocamlspot-message-init (buffer-file-name))
      ;; (ocamlspot-type-init) ; We must keep xtype buffer
      (ocamlspot-delete-overlays-now)
      (let ((query (ocamlspot-xtype-build-query-at-cursor)))
	(if query
	    (progn
	      (message "%s" query)
	      (ocamlspot-run-query '(query))
	      (ocamlspot-find-spot)
	      (ocamlspot-wait))
	  (message "query empty"))))
    ;; I dunno why but we need the following line to list-buffers work nicely
    (select-window sel-window)))

(defun ocamlspot-xtype-mode-map ()
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c;" 'ocamlspot-xtype-query)
    keymap))

(defun ocamlspot-process-mode-map ()
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c;" 'ocamlspot-browse-path-range-at-point)
    keymap))

(define-minor-mode ocamlspot-xtype-mode
  "OCamlSpot XType mode."
  :lighter " OCamlSpot-XType"
  :keymap (ocamlspot-xtype-mode-map))

(define-minor-mode ocamlspot-process-mode
  "OCamlSpot Process mode."
  :lighter " OCamlSpot-Process"
  :keymap (ocamlspot-process-mode-map))

(provide 'ocamlspot)
