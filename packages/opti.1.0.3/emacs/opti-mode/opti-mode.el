;; Opti mode based on http://www.emacswiki.org/emacs/DerivedMode

(require 'generic-x)

(defvar opti-tab-width 2 "Width of tab for Opti mode")

(define-generic-mode
  'opti-mode
  '() ;; comments start with ...
  '("range" "cname" "extern" "public" "private" "sets" "increments" "scales" "proc" "sum"
    "recomputes" "propagates" "delta" "unit" "min" "max" "abs" "float32" "float64") ;; keywords
  '(("=" . 'font-lock-operator-face)
    (";" . 'font-lock-builtin-face))
  '("\\.opti$")
  nil
  "A mode for Opti files")

(provide 'opti-mode)
