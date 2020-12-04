(define-generic-mode 'electrod-mode
	'("--") 
	'("check" "run" "invariant" "true" "false" "next" "always" "until" "releases"
		"previous" "historically" "once" "eventually" 
		"since" "all" "some" "one" "set" "no" "lone" "let" "disj" "iff" "implies"
		"then" "else" "or" "and" "in" "inst" "sym" "not" "var" "const" "univ" "iden" "none")
	'(("\$?[:alpha:]([:alnum:]|_|#)*(\$([:digit:]|[1-9][:digit:]*)?)" . 'font-lock-variable-name-face)
		("0|([1-9][:digit:])*" . font-lock-constant-face))
	'("\\.elo\\'")
	(list (lambda () (setq comment-start "--")))
	"Major mode for very simple Electrod highlighting.")
