;; Sturgeon implementation in Emacs.
;;
;; Naming convention
;;
;; By convention, all variables of the form sturgeon-... are targeted to users
;; of ;; the library while variables prefixed by sturgeon-- are internal to the
;; library: they might disappear or be changed without further notice.
;;

(require 'cl)

(defgroup sturgeon nil
  "`sturgeon' OCaml RPC"
  :group 'communication :prefix "sturgeon-")

(defcustom sturgeon-debug nil
  "If non-nil, every message, received or sent, will be dumped to *Message* buffer"
  :group 'sturgeon)

(defcustom sturgeon-connector "sturgeon-connector"
  "Commandline of the sturgeon-connector helper. Adjust it if the binary is not found in your path."
  :group 'sturgeon)

(defun sturgeon--debug (prefix content)
  "If `sturgeon-debug' is set, dump prefix and content to *Message* buffer"
  (when sturgeon-debug
    (let ((inhibit-message t))
      (message "%s %S" prefix content))))

(defvar sturgeon--filter-queue 'idle
  "Queue is filled with messages received from the process, and an
asynchonous handler pump and dispatch them.
Valid values are 'idle or a list of a (process . line) pair.
'idle means that no handler is running yet, so it should be spawned first.
List is the queue of waiting messages.  New ones should be prepended.
")

(defun sturgeon--filter-action ()
  "The message handler. It pump and dispatches messages from the queue."
  (let ((current-queue (nreverse sturgeon--filter-queue))
        (inhibit-quit t))
    (setq sturgeon--filter-queue 'idle)
    (let ((sturgeon--filter-queue nil)
          (proc nil) (lines nil))
      ;; Heuristic to make the interface responsive: accepting process output
      ;; now will fill the queue with buffered messages.  This is more
      ;; efficient than starting a new handler for each message and prevent
      ;; interfaces from flickering.
      ;; However, processing too many messages risk freezing the interfaces.
      ;; Hence it is called only once, before entering the dispatch loop.
      (accept-process-output)
      (while current-queue
        ;; Pump loop
        (dolist (item current-queue)
          (setq proc (car item))
          ;; Logic for concatening partial s-expressions,
          ;;
          (setq lines (split-string (cdr item) "\n"))
          (if (not (cdr lines))
              ;; Line is incomplete, append the chunk it to buffered prefix
              (process-put proc 'sturgeon-lines
                           (cons (car lines) (process-get proc 'sturgeon-lines)))
            ;; One or more lines were received
            (let ((line (cons (car lines) (process-get proc 'sturgeon-lines))))
              ;; Flush buffered prefix
              (setcar lines (apply 'concat (reverse line))))
            (let ((lines lines))
              ;; Put back last (unterminated or empty) chunk to buffered prefix
              (while (cddr lines) (setq lines (cdr lines)))
              (process-put proc 'sturgeon-lines (cdr lines))
              (setcdr lines nil))
            ;; Dispatch lines to user handler
            (with-demoted-errors "sturgeon: reading input %S"
              (dolist (line lines)
                (sturgeon--debug ">" line)
                (with-local-quit (sturgeon--handler proc line))))))
        ;; Pump messages that got queued during the loop
        (setq current-queue (nreverse sturgeon--filter-queue))
        (setq sturgeon--filter-queue nil)))))

(defun sturgeon--filter (proc lines)
  "Sturgeon process filter: start asynchonous handler if needed, queue lines."
  (when (eq sturgeon--filter-queue 'idle)
    (setq sturgeon--filter-queue nil)
    (run-at-time 0 nil #'sturgeon--filter-action))
  (setq sturgeon--filter-queue
        (cons (cons proc lines) sturgeon--filter-queue)))

;; Routines and macros that help implementing sturgeon continuations.
;;
;; To implement a <once> or a <many>, one can use some macros:
;;   (k-let-once var
;;      body)
;;   (k-let-many var
;;      body)
;;
;; These define a sturgeon continuation that can be send to remote side.  When
;; invoked, `body' is evaluated with `var' bound to the value of remote
;; argument.
;;
;; An extended form is accepted, to handle corner cases:
;;   (k-let-many v ; or k-let-once
;;      'quit
;;      (quit-body
;;          ;; v is bound to error message
;;      )
;;      body)
;;
;; Examples:
;;
;; (k-let-once v
;;    (message "remote sent value %S" v)
;;    (sturgeon-cancel v) ; we aren't doing anything with v,
;;                        ; let's release any sub-sessions asap
;;    )
;;
;; (k-let-once v
;;    'quit
;;    (if (eq v 'finalize)
;;        (message "once was collected before getting an answer")
;;      (message "once failed with message: %S" v))
;;
;;    (message "remote sent value %S" v)
;;    (sturgeon-cancel v)
;;    )
;;

(defun sturgeon--k-extract-handlers (body)
  "Extract other special cases from body, e.g. 'quit (quit-body)"
  (let ((handlers nil) (iter body))
    (while (symbolp (car iter))
      (setq handlers (cons
                      (cons `(eq sturgeon-kind ,(car iter)) (cadr iter))
                      handlers))
      (setq iter (cddr iter)))
    (cons handlers iter)))

(defmacro k-let-once (var &rest body)
  "A macro defining linear sturgeon continuations. See sturgeon.el for more documentation."
  (let ((handlers (sturgeon--k-extract-handlers body)))
    (setq body (cdr handlers))
    (setq handlers (car handlers))
    `(cons 'once (lambda (sturgeon-kind ,var)
                   (cond
                     ,@handlers
                     ((eq sturgeon-kind 'feed)
                      ,@body)
                     (t
                      (unless (eq sturgeon-kind 'quit)
                        (message "sturgeon: unhandled message %S %S" sturgeon-kind ,var))
                      (sturgeon-cancel ,var)))))))

(defmacro k-let-many (var &rest body)
  "A macro defining multi-shot sturgeon continuations. See sturgeon.el for more documentation."
  (let ((handlers (sturgeon--k-extract-handlers body)))
    (setq body (cdr handlers))
    (setq handlers (car handlers))
  `(cons 'many (lambda (sturgeon-kind sturgeon-values)
                 (cond
                   ,@handlers
                   ((member sturgeon-kind '(feed batch))
                    (when (eq sturgeon-kind 'feed)
                      (setq sturgeon-values (list sturgeon-values)))
                    (dolist (,var sturgeon-values)
                      (with-demoted-errors "sturgeon: error in multi-shot continuation %S"
                       ,@body)))
                   (t
                    (unless (eq sturgeon-kind 'quit)
                      (message "sturgeon: unhandled message %S %S" sturgeon-kind sturgeon-values))
                    (sturgeon-cancel sturgeon-values)))))))

(defmacro k-let-batch (var &rest body)
  "Don't use: WIP for receiving many values at once, might be removed in the future."
  (let ((handlers (sturgeon--k-extract-handlers body)))
    (setq body (cdr handlers))
    (setq handlers (car handlers))
  `(cons 'many (lambda (sturgeon-kind ,var)
                 (cond
                   ,@handlers
                   ((member sturgeon-kind '(feed batch))
                    (when (eq sturgeon-kind 'feed)
                      (setq ,var (list ,var)))
                    ,@body)
                   (t
                    (unless (eq sturgeon-kind 'quit)
                      (message "sturgeon: unhandled message %S %S" sturgeon-kind sturgeon-values))
                    (sturgeon-cancel sturgeon-values)))))))

(put 'k-let-once 'lisp-indent-function 'defun)
(put 'k-let-many 'lisp-indent-function 'defun)
(put 'k-let-batch 'lisp-indent-function 'defun)

(defun sturgeon--k-p (sexp)
  "Assertion validating well-formed sturgeon continuation"
  (and (consp sexp) (functionp (cdr sexp)) (member (car sexp) '(once many))))

(defun k-resume-once (f arg)
  "Resume a linear sturgeon continuation"
  (assert (sturgeon--k-p f))
  (assert (eq (car f) 'once))
  (funcall (cdr f) 'feed arg))

(defun k-resume (f arg)
  "Resume a sturgeon continuation (without checking linearity)"
  (assert (sturgeon--k-p f))
  (funcall (cdr f) 'feed arg))

(defun k-resume-many (f arg)
  "Resum a multi-shot sturgeon continuation"
  (assert (sturgeon--k-p f))
  (assert (eq (car f) 'many))
  (funcall (cdr f) 'feed arg))

(defun k-batch (f arg)
  "Resume a multi-shot sturgeon continuation multiple times values. WIP, might be removed in the future"
  (assert (sturgeon--k-p f))
  (assert (eq (car f) 'many))
  (funcall (cdr f) 'batch arg))

(defun k-quit (f &optional arg)
  "Quit a sturgeon continuation. An eventual error message can be provided to signal exceptional conditions."
  (assert (sturgeon--k-p f))
  (funcall (cdr f) 'quit arg))

;; Garbage collection

(defvar sturgeon--processes nil
  "A list of all sturgeon managed processes.
  Sturgeon processes are process objects with these properties:
    'sturgeon-lines
       unterminated command received from remote processes
    'sturgeon-cogreetings
       handler for the initial remote sessions
    'sturgeon-table
       the car is the seed of the local gensym.
       the cdr is table mapping addresses already generated and sent to the
       remote processes with the local handlers (once or many values)
    'sturgeon-roots
       remote roots retained locally
  ")

(defvar sturgeon--root-collection nil
  "nil when idle, or 'pending iff a sturgeon root collection has been scheduled")

(defun sturgeon--root-register (process addr value)
  "Register a remote value as a sturgeon root. When value is collected by
  local GC, addr can be released in remote process."
  (let* ((roots (process-get process 'sturgeon-roots))
           (addrs (car roots))
           (weaks (cdr roots)))
    (puthash addr t     addrs)
    (puthash addr value weaks)
    value))

(defun sturgeon--root-alive (process addr)
  "Return true iff root at addr is still alive"
  (gethash addr (car (process-get process 'sturgeon-roots))))

(defun sturgeon--root-remove (process addr)
  "Unregister a root. Doesn't do anything to remote process (root is not released)"
  (let* ((roots (process-get process 'sturgeon-roots))
         (addrs (car roots))
         (weaks (cdr roots)))
    (remhash addr addrs)
    (remhash addr weaks)))

(defun sturgeon--collect-roots ()
  "Collect all registered roots: if the local proxy was collected by emacs
  GC, roots are released in remote processes."
  (setq sturgeon--root-collection nil)
  (let ((sturgeon--root-collection t))
    (setq sturgeon--processes
          (delete-if (lambda (process)
                       (member (process-status process)
                               '(exit signal closed failed nil)))
                     sturgeon--processes))
    (dolist (process sturgeon--processes)
      (let* ((roots (process-get process 'sturgeon-roots))
             (addrs (car roots))
             (weaks (cdr roots)))
        (maphash
         (lambda (addr v)
           (unless (gethash addr weaks)
             (remhash addr addrs)
             (ignore-errors
               (sturgeon--send process (cons 'quit (cons addr 'finalize))))))
         addrs)))))

(defun sturgeon--gc-hook ()
  "Integrate sturgeon root collection to emacs GC"
  (unless sturgeon--root-collection
    (setq sturgeon--root-collection 'pending)
    (run-at-time 0 nil #'sturgeon--collect-roots)))

(add-hook 'post-gc-hook 'sturgeon--gc-hook)

;; Communication -- convert to and from extended s-exps

(defun sturgeon--register (process obj)
  "Allocate a local address for obj, register it with obj in dispatch table
  and return the identifier to be sent to remote process"
  (let* ((table (process-get process 'sturgeon-table))
         (key (car table)))
    (setcar table (1+ key))
    (puthash key obj (cdr table))
    (cons (car obj) key)))

(defun sturgeon-cancel (sexp)
  "Traverse the sexp and cancel all sessions it contains"
  (sturgeon--debug "cancelling" sexp)
  (cond
   ((sturgeon--k-p sexp)
    (with-demoted-errors "sturgeon: error cancelling continuation %S" (k-quit sexp 'cancel)))
   ((eq (car-safe sexp) 'meta) nil)
   ((consp sexp)
    (sturgeon-cancel (car sexp))
    (sturgeon-cancel (cdr sexp)))
   (t nil)))

(defun sturgeon--lower (process sexp)
  "Traverse sexp and lower all sessions it contains: they are turned into
  plain adresses, registered in dispatch table"
  (cond
   ((sturgeon--k-p sexp)
    (cons 'meta (sturgeon--register process sexp)))
   ((eq (car-safe sexp) 'meta)
    (cons 'meta (cons 'escape (cdr-safe sexp))))
   ((consp sexp)
    (cons (sturgeon--lower process (car sexp))
          (sturgeon--lower process (cdr sexp))))
   (t sexp)))

(defun sturgeon--lift (process kind addr)
  "Turn a remote address addr from process into a local <once> or <many>
  object (according to kind)"
  (lexical-let ((addr addr) (process process) (kind kind))
    (cons
     kind
     (sturgeon--root-register
      process addr
      (lambda (msg v)
        (cond
         ((not (sturgeon--root-alive process addr))
          (sturgeon-cancel v))
         ((eq msg 'quit)
          (sturgeon--root-remove process addr)
          (sturgeon--send process (cons 'quit (cons addr v))))
         (t
          (when (eq kind 'once) (sturgeon--root-remove process addr))
          (sturgeon--send process (cons 'feed (cons addr (sturgeon--lower process v)))))
         ))))))

(defun sturgeon--higher (process sexp)
  "Traverse sexp and lift all sessions it contains with `sturgeon--lift'.
  This is the operation dual to `sturgeon--lower'."
  (cond
   ((and (eq (car-safe sexp) 'meta)
         (eq (car-safe (cdr sexp)) 'escape))
    (cons 'meta (cddr sexp)))
   ((and (eq (car-safe sexp) 'meta)
         (member (car-safe (cdr sexp)) '(once many)))
    (sturgeon--lift process (cadr sexp) (cddr sexp)))
   ((consp sexp)
    (cons (sturgeon--higher process (car sexp))
          (sturgeon--higher process (cdr sexp))))
   (t sexp)))

(defun sturgeon--cancel-low (process sexp)
  "Traverse a sexp that has not yet been lifted to session and cancel all
  addresses from process. This is a lighter alternative to (`sturgeon-cancel' (`sturgeon--lift' ..))"
  (cond
   ((and (eq (car-safe sexp) 'meta)
         (eq (car-safe (cdr sexp)) 'escape))
    nil)
   ((and (eq (car-safe sexp) 'meta)
         (member (car-safe (cdr sexp)) '(once many)))
    (sturgeon--send process (cons 'quit (cons (cddr sexp) 'cancel))))
   ((consp sexp)
    (sturgeon--cancel-low process (car sexp))
    (sturgeon--cancel-low process (cdr sexp)))
   (t nil)))

;; Process management

(defun sturgeon--wake-up (process addr msg payload)
  "Handle negation level messages, then call user handler if appropriate"
  (let* ((table (process-get process 'sturgeon-table))
         (handler (gethash addr (cdr table)))
         (fn (cdr handler)))
    (when (or (eq 'once (car handler)) (eq msg 'quit))
      ; (message "REMOVE %S because %S %S" addr (cons (car handler) msg) payload)
      (remhash addr (cdr table)))
    (funcall fn msg payload)))

(defun sturgeon--handler (process answer)
  "Handle process level messages"
  (setq answer (car (read-from-string answer)))
  (let ((cmd (car-safe answer))
        (payload (cdr-safe answer)))
    (cond
     ((and (eq cmd 'greetings) (equal 1 (car-safe payload)))
      (with-demoted-errors "sturgeon: greetings %S"
        (let ((cogreetings (process-get process 'sturgeon-cogreetings)))
          (if (not cogreetings)
              (sturgeon--cancel-low process answer)
            (process-put process 'sturgeon-cogreetings nil)
            (funcall cogreetings (sturgeon--higher process (cdr payload)))))))
     ((eq cmd 'feed)
      (with-demoted-errors "sturgeon: feed %S"
        (sturgeon--wake-up process
                        (car payload) 'feed
                        (sturgeon--higher process (cdr payload)))))
     ((eq cmd 'quit)
      (with-demoted-errors "sturgeon: quit %S"
        (sturgeon--wake-up process
                        (car payload) 'quit (cdr payload))))

     ((eq answer 'end)
      ;; FIXME
      t)
     (t (sturgeon--cancel-low process answer)))))

(defun sturgeon--send (process command)
  "Serialize and send command to remote process"
  (setq command (prin1-to-string command))
  (setq command (replace-regexp-in-string "\n" "\\\\n" command))
  (sturgeon--debug "<" command)
  (process-send-string process (concat command "\n")))

;; Main functions

(defun sturgeon-start (process &rest rest)
  "Make process a sturgeon managed process. See `sturgeon-start-process' for optional arguments"
  (process-put process 'sturgeon-lines nil)
  (process-put process 'sturgeon-table (cons 0 (make-hash-table)))
  (process-put process 'sturgeon-cogreetings (plist-get rest :cogreetings))
  (process-put process 'sturgeon-roots
               (cons (make-hash-table)
                     (make-hash-table :weakness 'value)))
  (set-process-filter process #'sturgeon--filter)
  (setq sturgeon--processes (cons process sturgeon--processes))
  (sturgeon--send
   process
   (cons 'greetings (cons 1 (sturgeon--lower process (plist-get rest :greetings)))))
  process)

(defun sturgeon-start-process (name buffer path args &rest rest)
  "Start a sturgeon managed process.
Optional arguments are:
  :greetings greetings-session
  :cogreetings function-that-will-receive-remote-greetings
"
  (let* ((start-file-process
          (if (fboundp 'start-file-process)
              #'start-file-process
            #'start-process))
         ;; Invoke with a pipe rather than a pty
         (process-connection-type nil)
         (process (apply start-file-process name buffer path args)))
    (apply 'sturgeon-start process rest)))

;; Rich printing facility

(require 'button)

(defvar-local sturgeon-ui--cursor nil)
(defvar-local sturgeon-ui--revision 0)
(defvar-local sturgeon-ui--point-moved nil)
(defvar-local sturgeon-ui--last-point 0)
(defconst sturgeon-ui--active nil)

;; cursor = [0:buffer 1:remote-cont 2:remote-revision 3:changes 4:latest-remote]
(defun sturgeon-ui--change-cursor (cursor beg end len)
  (setq beg (1- beg))
  (setq end (1- end))
  ;; Record changes
  (let ((changes (elt cursor 3)))
    (unless (equal len 0)
      (setq changes
            (cons (vector sturgeon-ui--revision 'remove beg len) changes)))
    (unless (equal beg end)
      (setq changes
            (cons (vector sturgeon-ui--revision 'insert beg (- end beg)) changes)))
    (aset cursor 3 changes))
  ;; Commit changes
  (let* ((text (encode-coding-string
                (buffer-substring-no-properties (1+ beg) (1+ end))
                'utf-8 t))
         (op (cond
              ((equal (length text) 0)
               (cons 'remove len))
              ((equal len 0)
               (cons 'insert (cons text (length text))))
              (t
               (cons 'replace (cons len (cons text (length text)))))))
         (action (list 'patch
                       (cons (elt cursor 2) sturgeon-ui--revision)
                       beg op
                       (cons 'editable nil))))
    (aset cursor 4 sturgeon-ui--revision)
    (k-resume-many (elt cursor 1) action)))

(defun sturgeon-ui--before-change (beg end)
  (when sturgeon-ui--cursor
    (unless sturgeon-ui--active
      (setq sturgeon-ui--last-point (point)))))

(defun sturgeon-ui--after-change (beg end len)
  (when sturgeon-ui--cursor
    (setq sturgeon-ui--revision (1+ sturgeon-ui--revision))
    (unless sturgeon-ui--active
      (sturgeon-ui--change-cursor sturgeon-ui--cursor beg end len))))

(defun sturgeon-ui--update-revisions (cursor revisions)
  (aset cursor 2 (cdr revisions))
  (let ((pred (lambda (change) (<= (elt change 0) (car revisions)))))
    (aset cursor 3 (delete-if pred (elt cursor 3))))
  (when (< (+ 16 (elt cursor 4)) (cdr revisions))
    (aset cursor 4 (cdr revisions))
    (k-resume-many
     (elt cursor 1)
     `(ack ,(cons (cdr revisions) sturgeon-ui--revision)))))

(defun sturgeon--remap (s l x)
  (if (< x s) x
    (if (> x (+ s l))
      (- x l)
      s)))

(defun sturgeon--commute-op (cursor k2 s2 l2)
  (dolist (op1 (reverse (elt cursor 3)))
    (let* ((k1 (elt op1 1))
           (s1 (elt op1 2))
           (l1 (elt op1 3)))
      ; (message "commuting %S %S" k1 k2)
      (cond
       ((and (eq k1 'remove) (eq k2 'remove))
        (let ((e2 (sturgeon--remap s1 l1 (+ s2 l2))))
          (setq s2 (sturgeon--remap s1 l1 s2))
          (setq l2 (- e2 s2))))

       ((and (eq k1 'remove) (eq k2 'insert))
        (unless (< s2 s1)
          (if (< s2 (+ s1 l1))
              (setq l2 0)
            (setq s2 (- s2 l1)))))

       ((and (eq k1 'insert) (eq k2 'remove))
        (if (< s1 s2)
            (setq s2 (+ s2 l1))
          (if (< s1 (+ s2 l2))
              (setq l2 (+ l2 l1)))))

       ((and (eq k1 'insert) (eq k2 'insert))
        (if (< s1 s2)
          (setq s2 (+ s2 l1))))

       (t
         (message "sturgeon--commute-op: k1:%S k2:%S" k1 k2)
         (assert nil)))))
  (cons s2 l2))

(defun sturgeon-ui--cursor-action (x)
  (let* ((cursor (button-get x 'sturgeon-cursor))
         (cont   (elt cursor 1))
         (rev    (cons (elt cursor 2) sturgeon-ui--revision))
         (offset (1- (marker-position x)))
         (action `(patch ,rev ,offset (propertize . 0) (clicked))))
    (k-resume-many cont action)))

(defun sturgeon-ui--propertize (cursor offset len flags)
  (let ((custom-prop (cdr-safe (assoc 'custom flags))))
    (when custom-prop (add-text-properties offset (+ offset len) flags)))
  (when (member 'focus flags)
    (goto-char offset)))

(defun sturgeon-ui--substitute (cursor offset oldlen text newlen flags)
  (let ((point-begin (point))
        (inhibit-read-only t)
        (inhibit-point-motion-hooks t)
        (sturgeon-ui--active t))
   (save-excursion
     (when (> oldlen 0)
       (let ((pos (sturgeon--commute-op cursor 'remove offset oldlen)))
        (when (> (cdr pos) 0)
          (goto-char (1+ (car pos)))
          (delete-char (cdr pos) nil))))
     (when (and text (> (length text) 0))
       (let ((pos (sturgeon--commute-op cursor 'insert offset (length text))))
        (when (> (cdr pos) 0)
          (unless (member 'raw flags)
            (setq text (decode-coding-string text 'utf-8 t)))
          (when (and (member 'prompt flags) (> (length text) 0))
            (setq text (propertize text 'read-only t 'intangible t))
            (add-text-properties (1- (length text)) (length text)
                                 '(rear-nonsticky
                                   (intangible sturgeon-actionable read-only)
                                   sturgeon-actionable after)
                                 text))
          (if (member 'editable flags)
              (setq text (propertize text 'sturgeon-actionable t))
            (setq text (propertize text 'read-only t)))
          (let ((custom-prop (cdr-safe (assoc 'custom flags))))
            (when custom-prop (setq text (apply 'propertize text custom-prop))))
          (goto-char (1+ (car pos)))
          (if (member 'clickable flags)
              (insert-text-button
               (propertize text 'sturgeon-actionable t)
               'action 'sturgeon-ui--cursor-action
               'sturgeon-cursor cursor)
             (insert text))))))

   ;; Heuristics to place point at natural positions

   ;; (message "%d: removed %d inserted %d %S, point %d moved to %d, last user point was %d, last forced move was %d to %d"
   ;;          (1+ offset) oldlen newlen text point-begin (point)
   ;;          sturgeon-ui--last-point
   ;;          (or (car-safe sturgeon-ui--point-moved) 0) (or (cdr-safe sturgeon-ui--point-moved) 0))

   ;; First check: user removed a character that sturgeon reinserted
   ;;              immediately after
   ;; (when (and (equal (point) point-begin)
   ;;            (equal point-begin (1+ offset))
   ;;            (equal newlen 1)
   ;;            (equal sturgeon-ui--last-point (1+ point-begin)))
   ;;   ;; (message "MOVE TO %d" sturgeon-ui--last-point)
   ;;   (goto-char sturgeon-ui--last-point))

   ;; ;; Second check: sturgeon reinserted data it had removed at a place
   ;; ;;               where the cursor was and had to be moved.
   ;; (if (not (equal (point) point-begin))
   ;;     (setq sturgeon-ui--point-moved (cons point-begin (point)))
   ;;   (if (equal point-begin (cdr-safe sturgeon-ui--point-moved))
   ;;     (progn
   ;;       (goto-char (min (+ 1 offset newlen) (car-safe sturgeon-ui--point-moved)))
   ;;       (setq sturgeon-ui--point-moved (cons point-begin (car-safe sturgeon-ui--point-moved))))
   ;;     ))
   ;; ))
   ))

(push 'sturgeon-actionable text-property-default-nonsticky)

(defun sturgeon-ui--apply-patch (cursor value)
  (let* ((buffer    (elt cursor 0))
         (revisions (elt value 1))
         (offset    (elt value 2))
         (operation (elt value 3))
         (kind      (car operation))
         (flags     (elt value 4)))
    (sturgeon-ui--update-revisions cursor revisions)
    (with-current-buffer buffer
      (cond
        ((eq kind 'propertize)
         (sturgeon-ui--propertize cursor offset (cdr operation) flags))
        ((eq kind 'remove)
         (sturgeon-ui--substitute cursor offset (cdr operation) "" 0 flags))
        ((eq kind 'insert)
         (sturgeon-ui--substitute cursor offset 0
                                  (cadr operation) (cddr operation) flags))
        ((eq kind 'replace)
         (sturgeon-ui--substitute cursor offset (cadr operation)
                                  (caddr operation) (cdddr operation) flags))
      ))))

(define-derived-mode sturgeon-mode fundamental-mode "Sturgeon"
   "A major mode for Sturgeon managed buffers"
   (buffer-disable-undo)
   (add-hook 'before-change-functions 'sturgeon-ui--before-change nil 'local)
   (add-hook 'after-change-functions 'sturgeon-ui--after-change nil 'local))

(defun sturgeon-ui--manage-buffer (buffer cont)
  (lexical-let ((cursor (vector buffer cont 0 nil 0)))
    (with-current-buffer buffer
      (erase-buffer)
      (sturgeon-mode)
      (setq sturgeon-ui--cursor cursor)
      (k-let-many value
        (cond
         ((eq (car value) 'split)
          (let ((split (cadr value))
                (buffer (get-buffer-create
                          (generate-new-buffer-name (caddr value))))
                (cont (cadddr value))
                (window (or (get-buffer-window (elt cursor 0)) (selected-window))))
            (cond
              ((eq split 'left) (split-window-horizontally))
              ((eq split 'top) (split-window-vertically))
              ((eq split 'right) (setq window (split-window-horizontally)))
              ((eq split 'bottom) (setq window (split-window-vertically))))
            (with-selected-window window
             (switch-to-buffer buffer)
             (k-resume cont
              (cons 'many (sturgeon-ui--manage-buffer buffer cont))))))
         ((eq (car value) 'fit)
          (let ((window (get-buffer-window (elt cursor 0)))
                (fit-window-to-buffer-horizontally t))
            (when window
              (with-selected-window window (fit-window-to-buffer)))))
         ((eq (car value) 'ack)
          (let* ((revisions (cadr value)))
            (sturgeon-ui--update-revisions cursor revisions)))
         ((eq (car value) 'patch)
          (sturgeon-ui--apply-patch cursor value)))))))

(defun sturgeon-ui-handler (value &optional buffer)
  (let ((cmd (car-safe value)))
    (cond ((eq cmd 'create-buffer)
           (let ((name (cadr value))
                 (cont (caddr value)))
             (if (and buffer (not sturgeon-ui--cursor))
                 (with-current-buffer buffer (rename-buffer name t))
               (setq buffer
                     (get-buffer-create (generate-new-buffer-name name))))
             (k-resume cont
              (cons 'many (sturgeon-ui--manage-buffer buffer cont)))))
          ((eq cmd 'message)
           (message "%s" (cadr value)))
          ((eq cmd 'popup-menu)
           (let ((title    (elt value 1))
                 (items    (elt value 2))
                 (callback (elt value 3)))
             (k-resume-once callback
                       (with-local-quit
                         (popup-menu (easy-menu-create-menu title items))))))
          ((eq cmd 'read-file-name)
           (let ((prompt   (elt value 1))
                 (dir      (elt value 2))
                 (default  (elt value 3))
                 (callback (elt value 4))
                 (use-dialog-box nil)
                 filename)
             (setq filename
                   (with-local-quit (read-file-name prompt dir default)))
             (when filename (setq filename (expand-file-name filename))
             (k-resume-once callback filename))))
          (t (sturgeon-cancel value)))))

(defun sturgeon-ui-cogreetings (buffer)
  (lexical-let* ((buffer buffer))
    (lambda (value)
      (cond
       ((eq (car-safe value) 'buffer-shell)
        (k-resume-once (cadr value)
                  (k-let-many value
                   (sturgeon-ui-handler value buffer))))
       (t (sturgeon-cancel value))
       ))))

;;;###autoload
(defun sturgeon-launch (filename)
  (interactive "fProgram path: ")
  (let ((buffer (get-buffer-create filename)))
    (sturgeon-start-process
     filename buffer
     filename nil
     :cogreetings (sturgeon-ui-cogreetings buffer))
    (switch-to-buffer buffer)))

;; Resolve path of sturgeon-connector

(defun sturgeon--look-for-relative-connector ()
  "Compute path of sturgeon-connector binary relative to sturgeon.el when installed by opam"
  (let ((p (find-lisp-object-file-name 'sturgeon-mode nil)))
    ;; p ~ prefix/share/emacs/site-lisp/sturgeon.el
    (when p (setq p (directory-file-name (file-name-directory p))))
    ;; p ~ prefix/share/emacs/site-lisp
    (when p (setq p (directory-file-name (file-name-directory p))))
    ;; p ~ prefix/share/emacs
    (when p (setq p (directory-file-name (file-name-directory p))))
    ;; p ~ prefix/share
    (when p (setq p (file-name-directory p)))
    ;; p ~ prefix/
    (when p (setq p (file-name-as-directory (concat p "bin"))))
    ;; p ~ prefix/bin
    (when p (setq p (concat p "sturgeon-connector")))
    ;; p ~ prefix/bin/sturgeon-connector
    (customize-set-variable 'sturgeon-connector p)))

(defun sturgeon--configure-connector (path)
  (interactive
   (list (read-file-name "Specify path to connector command (sturgeon-connector): "
                         nil "sturgeon-connector" t
                         (executable-find "sturgeon-connector") 'file-executable-p)))
  (when (not (file-executable-p path))
    (error "Selected sturgeon-connector (%s) is not executable." path))
  (customize-set-variable 'sturgeon-connector path)
  (message "Customized sturgeon-connector to %s" path))

(defun sturgeon--process-lines (&rest args)
  (let ((lines (apply 'process-lines args)) header)
    (setq header (car-safe lines))
    (unless (and (stringp header)
                 (string-match "^# sturgeon-connector " header))
      (error "sturgeon-connector gave invalid handshake (%S), check version." header))
    (cdr lines)))

;;;###autoload
(defun sturgeon-connect (name)
  (interactive
   (list (completing-read
          "Socket: "
          (condition-case nil (sturgeon--process-lines sturgeon-connector "list")
            (error
             (let ((inhibit-message t))
               (message "Cannot find 'sturgeon-connector' command, resolving relatively to sturgeon.el"))
             (sturgeon--look-for-relative-connector)
             (condition-case nil (sturgeon--process-lines sturgeon-connector "list")
               (error
                (let ((inhibit-message t))
                  (message "Cannot find 'sturgeon-connector' command, prompting user (sturgeon--configure-connector)"))
                (call-interactively 'sturgeon--configure-connector)
                (sturgeon--process-lines sturgeon-connector "list"))))))))
  (let ((buffer (get-buffer-create name)))
    (if (and (boundp 'sturgeon--remote) sturgeon--remote)
        (sturgeon-start-process
          name buffer
          sturgeon-connector (list "pipe" name)
          :cogreetings (sturgeon-ui-cogreetings buffer))
      (let ((path (or (car-safe (process-lines sturgeon-connector "which" name)) name)))
         (sturgeon-start (make-network-process
                          :name name :buffer buffer :family 'local :service path)
                         :cogreetings (sturgeon-ui-cogreetings buffer))))
      (switch-to-buffer buffer)))

;;;###autoload
(defun sturgeon-remote-launch (server)
  (interactive "fServer: ")
  (let ((default-directory server)
        (sturgeon--remote t))
    (call-interactively 'sturgeon-launch)))

;;;###autoload
(defun sturgeon-remote-connect (server)
  (interactive "fServer: ")
  (let ((default-directory server)
        (sturgeon--remote t))
    (call-interactively 'sturgeon-connect)))

;;;###autoload
(defun sturgeon-previous-actionable ()
  (interactive)
  (let (point)
    (setq point (previous-single-property-change (point) 'sturgeon-actionable))
    (when (and point (not (get-text-property point 'sturgeon-actionable)))
      (setq point (or (previous-single-property-change point 'sturgeon-actionable)
                      point)))
    (when point
      (goto-char point))))

;;;###autoload
(defun sturgeon-next-actionable ()
  (interactive)
  (let (point)
    (setq point (next-single-property-change (point) 'sturgeon-actionable))
    (when (and point (not (get-text-property point 'sturgeon-actionable)))
      (setq point (or (next-single-property-change point 'sturgeon-actionable)
                      point)))
    (when point
      (if (eq (get-text-property point 'sturgeon-actionable) 'after)
        (setq point (1+ point)))
      (goto-char point))))

(define-key sturgeon-mode-map (kbd "TAB") 'sturgeon-next-actionable)
(define-key sturgeon-mode-map (kbd "<backtab>") 'sturgeon-previous-actionable)

;; Done

(provide 'sturgeon)
