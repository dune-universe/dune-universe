;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Usage

; YOU NEED EMACS 24

; in .emacs:
;   (add-to-list 'load-path "THE DIRECTORY THIS ELISP EXISTS")
;   (require 'dune-mode)
;   (setq dune-program-path "/dune/or/jdune/path")
;   (setq dune-program-arguments "-P -w -j 3 --verbose")
;      ; dune command options. -w and --verbose are required for error browsing
;   (setq dune-error-highlight-background "#880000")
;
;   ; key bindings (jfuruse's setting)
;   (global-unset-key "\M-P") ; Shift+Alt+p 
;   (global-unset-key "\M-N") ; Shift+Alt+n
;   (global-unset-key "\M-o") ; Alt+o
;   (global-unset-key "\M-O") ; Shift+Alt+o
;   (global-unset-key [(control shift o)]) ; Ctrl+Shift+o
;
;   (global-set-key "\M-O" 'dune-run)
;      ; launch a new dune process in the current buffer's directory
;
;   (global-set-key [(control shift o)] 'dune-rerun)
;      ; restart dune process of the current dune buffer
;
;   (global-set-key "\M-P" 'dune-previous-error)
;   (global-set-key "\M-N" 'dune-next-error)
;      ; visit the previous/next error of the current dune window
;
;   (global-set-key "\M-o" 'dune-round-visit-buffer)
;      ; visit another dune process window
;
;   ; aother possibilities
;   (global-unset-key [M-up])
;   (global-unset-key [M-down])
;   (global-set-key [M-up] 'dune-previous-error)
;   (global-set-key [M-down] 'dune-next-error)

(require 'cl)
(require 'font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Configurables

; dune path and args
(defconst dune-program-path "dune_watch")
; (defconst dune-program-arguments "-P -w -j 3 --verbose")
(defconst dune-program-arguments "")

; colors
(defconst dune-error-highlight-background "#FFFF00")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Notification

;; notifiaction

;; You can make some silly sounds
;;
;; (defun dune-play-sound (file)
;;   (start-process "dune-sound" dune-misc-buffer-name "afplay" file))
;; 
;; (defun dune-notify (k)
;;   (case k
;;     ('start   (dune-play-sound "/System/Library/Sounds/Frog.aiff"))
;;     ('success (dune-play-sound "/System/Library/Sounds/Pop.aiff"))
;;     ('error   (dune-play-sound "/System/Library/Sounds/Sosumi.aiff"))
;;     ))

(defun dune-notify (k) ) ; do nothing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst dune-buffer-name "*dune*")
(defconst dune-buffer-pattern "^\*dune\*")
(defconst dune-misc-buffer-name "*dune-misc*")

;; OCaml error. XXX should support Dune error lines
(defconst dune-error-regexp "File \"\\(.*\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\)")

; (defconst dune-progress-regexp "\\(\\[\\([= ]+\\|saved \\.dunedb[= ]*\\)\\] [0-9]+ / [0-9]+\\)")
(defconst dune-progress-regexp "\\(\\[\\([= ]+\\|saved \\.dunedb[= ]*\\)\\] [0-9]+ / [0-9]+\\)")

(defconst dune-directory-regexp "- exit \\([^ ]+\\)")

(defconst dune-report-regexp "dune_watch:")

(defconst dune-rebuild-regexp "dune_watch: start:")
; (defconst dune-rebuild-regexp "\\*\\*\\* dune: \\(rebuilding\\|reading Dunefiles$\\)")

(defconst dune-locked-regexp "\\*\\*\\* dune: waiting for project lock:")

(defconst dune-root-regexp "\\*\\*\\* dune: changing directory to \\(.*\\)")

(defvar dune-buffers nil)

;; overlay
(setq dune-overlay-log nil)
(setq dune-overlay-source nil)

(defun dune-create-overlay ()
  (let ((overlay (make-overlay 1 1 nil)))
    (make-face 'dune-face)
    (set-face-doc-string 'dune-face "face for dune highlight")
    (set-face-background 'dune-face dune-error-highlight-background)
    (overlay-put overlay 'face 'dune-face)
    overlay))

(defun dune-display-overlay-log (buffer start end)
  (interactive)
  (if (not (overlayp dune-overlay-log))
      (setq dune-overlay-log (dune-create-overlay)))
  (move-overlay dune-overlay-log start end buffer))

(defun dune-display-overlay-source (buffer start end)
  (interactive)
  (if (not (overlayp dune-overlay-source))
      (setq dune-overlay-source (dune-create-overlay)))
  (move-overlay dune-overlay-source start end buffer))

;; CR jfuruse: use the variable above!
(defconst dune-font-lock-keywords
  (list 
   (cons dune-error-regexp font-lock-warning-face)
   (cons dune-progress-regexp font-lock-doc-face)
   (cons dune-directory-regexp font-lock-function-name-face)
   (cons dune-report-regexp font-lock-type-face)))


(defun dune-fold (f st lst)
  (if lst (dune-fold f (funcall f st (car lst)) (cdr lst))
      st))

;; oh shit, we really have no recursion in elisp
;; (defun dune-filter (p lst)
;;   (if lst
;;       (if (funcall p (car lst))
;;           (cons (car lst) (dune-filter p (cdr lst)))
;;         (dune-filter p (cdr lst)))
;;     nil))

(defun dune-filter-rev (p lst)
  (let ((res nil))
    (mapcar (lambda (x) (if (funcall p x) (setq res (cons x res)))) lst)
    res))

(defun dune-filter (p lst)
  (reverse (dune-filter-rev p lst)))
  
(defun dune-filter-map (p lst)
  (if lst
      (let ((x (funcall p (car lst))))
        (if x (cons x (dune-filter-map p (cdr lst)))
          (dune-filter-map p (cdr lst))))
    nil))

(defun dune-round-buffers ()
  (if dune-buffers 
      (let ((res (car dune-buffers)))
        (setq dune-buffers (cdr dune-buffers))
        res)
    (setq dune-buffers 
          (dune-filter (lambda (buf) 
                          (string-match "\\*dune\\*" (buffer-name buf)))
                        (buffer-list)))
    (if dune-buffers (dune-round-buffers))))

(defvar dune-current-buffer "")

(defun dune-display-buffer (buf)
  (interactive)
  (display-buffer buf t)
  (with-selected-window (get-buffer-window (current-buffer))
    (recenter -1))
  (setq dune-current-buffer buf)
  (save-current-buffer
    (set-buffer buf)))

(defun dune-round-visit-buffer ()
  (interactive)
  (let ((buf (dune-round-buffers)))
    (if buf (dune-display-buffer buf))
    buf))

(defun dune-set-font-lock ()
  (setq font-lock-defaults 
        '(dune-font-lock-keywords t ; keyword only
                                   nil nil nil))
  (font-lock-mode 1))

(defun dune-create-buffer (dir)
  (let ((buffer-name (buffer-name (generate-new-buffer dune-buffer-name))))
    (set-buffer buffer-name)
    (cd dir)
    (dune-mode)
    (dune-set-font-lock)
    (make-local-variable 'last-progress-point)
    (setq last-progress-point nil)
    (make-local-variable 'remained-output)
    (setq remained-output nil)
    (make-local-variable 'last-line-was-end-of-build)
    (setq last-line-was-end-of-build nil)
    (make-local-variable 'no-error)
    (setq no-error t)
    (make-local-variable 'root-dir)
    (setq root-dir dir)
    buffer-name))

(defun dune-insert-line (string)
  (save-excursion
    (goto-char (point-max))
    (insert-before-markers string) ; (insert string)
    ))

(defun dune-insert-progress (string)
  (if string 
      (save-excursion
        (goto-char (point-max))
        (setq last-progress-point (point))
        (insert-before-markers string) ; (insert string)
        (string-match " [0-9]+ / [0-9]+" string)
        (setq mode-line-process (replace-match "" t nil string)))
    (setq mode-line-process nil)))

;; place holder for last-progress-meter
;; CR jfuruse: this must be buffer specific
(setq last-progress-meter nil)

(defun dune-process-filter (process output)
  (save-current-buffer
    (let ((buffer (process-buffer process)))
      ; if buffer is gone, we do nothing.
      (if buffer 
          (progn
            (set-buffer buffer)

            ;; concat remained output
            (if remained-output
                (setq output (concat remained-output output)))
            (setq remained-output nil)

            ;; if we print progress meter in the last call, delete it
            (if last-progress-point
                (save-excursion
		  (delete-region last-progress-point (point-max))))

; We get something like	    
; [===========================================                   ] 00910 / 01300                                                                                 -build ocamltter/lib utf16.cmi

;	    ;; Each line end is . Fix  => ''
;            (while (string-match "" output) 
;              (setq output (replace-match "\n" t nil output)))

	    ;; OS X, we see  before each new line.  Must be replaced with ''
            (while (string-match "" output) 
              (setq output (replace-match "" t nil output)))

	    ;; remove progress meter string from log
	    (while (string-match dune-progress-regexp output)
	      ;; set the match to last-progress-meter
	      ;; remove ^M at the end
	      (setq last-progress-meter (match-string 1 output))
	      
	      (setq output (replace-match "" t nil output)))

            ;; print output per line
            (while (string-match ".*\n" output)
        
              ;; clear if a new make started
              (if last-line-was-end-of-build
		  (progn
		    (erase-buffer)
		    (dune-insert-line "dune_watch: new dune started\n")
		    ))

              ;; get the line
              (setq line (match-string 0 output))
              (setq output (substring output (match-end 0)))
              ;; and print 
              (dune-insert-line line)
	      
              ;; root
              (if (string-match dune-root-regexp line)
                  (setq root-dir (match-string 1 line)))

              ;; error lines
              (if (string-match dune-error-regexp line)
                  (progn
                    (dune-notify 'error)
                    (setq no-error nil)
                    (dune-display-buffer buffer)))

              ;; locked
              (if (string-match dune-locked-regexp line)
                  (progn
		    (dune-notify 'error)
                    (setq no-error nil)
                    (dune-display-buffer buffer)))
              
              (if (string-match dune-rebuild-regexp line)
                  (progn
		    (dune-notify 'start)
		    ))
                
              ;; find the end of build
              (setq last-line-was-end-of-build
                    (string-match "dune_watch: end" line))
              (if last-line-was-end-of-build
                  (progn
                    (setq last-progress-meter nil)
                    (dune-insert-line "dune_watch: WAITING YOUR CHANGE\n")
                    (if no-error (dune-notify 'success))
                    (setq no-error t)
                    (dune-display-buffer buffer)))
              )
      
            ;; if something left, it is not ended with \n. Keep it
            (setq remained-output output)

            (setq last-progress-point nil)
            (dune-insert-progress last-progress-meter)
	    )))))

(defun dune-find-file-existing (path)
  (if (file-exists-p path)
      (find-file path) ; CR find-file-literary ?
    (progn (message "ERROR: source file %s was not found" path)
           nil)))

(defun dune-display-error (dir file line char-start char-end)
  (let 
      ((path (concat (file-name-as-directory (concat (file-name-as-directory root-dir) dir)) file)))
    (message path)
    (setq target-buffer (dune-find-file-existing path))
    (if target-buffer
        (progn 
          (goto-line line)
          (let* 
              ((char-of-line (line-beginning-position))
               (char-of-start (+ char-of-line char-start))
               (char-of-end (+ char-of-line char-end)))
            (dune-display-overlay-source (current-buffer) char-of-start char-of-end)
            (goto-char char-of-start))
          target-buffer)
      nil)))

(defun dune-jump-error (next)
  (if (get-buffer dune-current-buffer)
      (progn 
        (set-buffer dune-current-buffer)
        (if next (move-end-of-line nil)
          (move-beginning-of-line nil))
        (display-buffer dune-current-buffer t)
        (let ((found-start -1)
              (found-end -1)
              (dir "")
              (file "")
              (line "")
              (char-start -1)
              (char-end -1)
              (window (get-buffer-window (current-buffer))))
          (if (progn
                (if 
                    (if next
                        ; sometimes the error has tab in its head...
                        (re-search-forward dune-error-regexp
                                           nil ; CR BOUND can be used to avoid the summary 
                                           t ; ignore not found error and return simply nil
                                           )
                      (re-search-backward dune-error-regexp
                                          nil t))
                    (progn
                      (setq found-start (match-beginning 0))
                      (setq found-end (match-end 0))
                      (setq file (match-string 1))
                      (setq line (string-to-int (match-string 2)))
                      (setq char-start (string-to-int (match-string 3)))
                      (setq char-end (string-to-int (match-string 4)))
                      (set-window-point window (if next found-end found-start))
                      (save-excursion
;                        (if (re-search-backward "Entering directory `\\(.*\\)'" nil t)
                        (if (re-search-forward dune-directory-regexp nil t)
                            (progn 
                              (setq dir (match-string 1))
                              t)
                          (progn 
                            (message "Error message found but no directory info")
                            "."))))
                  (progn
                    (message "No more error found")
;                    (delete-overlay dune-overlay-log)
;                    (delete-overlay dune-overlay-source)
                    nil)))
              (progn ; search successful: highlight the error line
                (save-current-buffer (dune-display-error dir file line char-start char-end))
                (dune-display-overlay-log (current-buffer) found-start found-end)
                ))))
    (message "no dune buffer selected")))

(defun dune-next-error () 
  (interactive)
  (dune-jump-error t))

(defun dune-previous-error () 
  (interactive)
  (dune-jump-error nil))

(defun dune-mode ()
  (interactive)
  (setq major-mode 'dune-mode)
  (setq mode-name "dune mode"))

(defun dune-run-dir (dir)
  (interactive)
  (save-current-buffer
    (setq debug-on-error t)
    (let* ((buffer (dune-create-buffer dir))
           (process 
            (start-process-shell-command buffer buffer 
                                         dune-program-path
                                         dune-program-arguments)))
      (setq last-progress-meter nil)
      (set-process-filter process 'dune-process-filter)
      (dune-display-buffer buffer)
      )))

(defun dune-run ()
  (interactive)
  (dune-run-dir default-directory))

(defun dune-rerun ()
  (interactive)
  (if dune-current-buffer
      (if (get-buffer dune-current-buffer)
          (let ((old-buffer dune-current-buffer)
                (dir (save-current-buffer
                       (set-buffer dune-current-buffer)
                       default-directory)))
            (dune-run-dir dir)
            (kill-buffer old-buffer))
        (message "no current dune buffer"))
    (message "no current dune buffer")))

(provide 'dune-mode)
