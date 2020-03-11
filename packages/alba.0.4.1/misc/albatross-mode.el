;;  INSTALLATION
;;    To install, simply copy this file into a directory in your
;;    load-path and add the following commands in your .emacs file:
;;
;;    (add-to-list 'auto-mode-alist '("\\.al\\'"  . albatross-mode))
;;    (add-to-list 'auto-mode-alist '("\\.ali\\'" . albatross-mode))
;;    (autoload 'albatross-mode "albatross-mode"
;;              "Major mode for Albatross programs" t)


(defconst alba-keywords
  '( "agent"    "all"      "and"         "as"         "assert"
     "attribute"
     "case"     "check"       "class"      "create"
     "deferred" "do"
     "feature"  "from"
     "ghost"
     "else"     "elseif"      "end"        "ensure"
     "if"       "immutable"   "in"         "inherit"    "inspect"    "invariant"
     "local"
     "mod"
     "not"      "note"
     "old"      "or"          "orif"
     "proof"
     "redefine" "rename"      "require"
     "Result"
     "some"
     "then"
     "undefine" "until"    "use"
     "variant"  "via"
     "while"
    ))


(defconst alba-keywords-regexp (regexp-opt alba-keywords 'words))

(defconst alba-font-lock-keywords
  `(
    (,"-- .*$"                . font-lock-comment-face)
    (,alba-keywords-regexp     . font-lock-keyword-face)
    ;(,"[A-Za-z][A-Za-z0-9_]+" . font-lock-variable-name-face)
    (,"\\<false\\>\\|\\<true\\>\\|\\<[0-9]+\\>"  . font-lock-constant-face)
    ))

; font-lock-warning-face
; font-lock-function-name-face,
; font-lock-comment-delimiter-face
; font-lock-type-face
; font-lock-builtin-face
; font-lock-preprocessor-face
; font-lock-string-face
; font-lock-doc-face
; font-lock-negation-char-face

(defvar alba-syntax-table (make-syntax-table))

(modify-syntax-entry ?_  "w  "  alba-syntax-table)
(modify-syntax-entry ?\( "()  " alba-syntax-table)
(modify-syntax-entry ?\) ")(  " alba-syntax-table)
(modify-syntax-entry ?\[ "(]  " alba-syntax-table)
(modify-syntax-entry ?\] ")[  " alba-syntax-table)
(modify-syntax-entry ?\{ "(}  " alba-syntax-table)
(modify-syntax-entry ?\} "){  " alba-syntax-table)

(modify-syntax-entry ?\{ "()1n"
                     alba-syntax-table) ; '{' is first character of comment start

(modify-syntax-entry ?:  ". 23n"
                     alba-syntax-table) ; ':' is second character of comment start,
                                        ; and first character of comment end
(modify-syntax-entry ?\} ")(4" alba-syntax-table)
                                        ; '}' is last character of comment end


(define-derived-mode albatross-mode fundamental-mode "albatross"
  (setq font-lock-defaults '(alba-font-lock-keywords))
  (setq mode-name "albatross")
  (set-syntax-table alba-syntax-table)
  (define-key albatross-mode-map "\C-c\C-c" 'compile)
  )


(provide 'albatross-mode)
