.PHONY: test
test: test1.ml
	@ocaml-expect test1.ml
	@ocaml-expect test2-fail.ml &> /dev/null || true
	@diff -u test2-fail.ml.corrected.expected test2-fail.ml.corrected
	@ocaml-expect -sexp test-sexp-output.ml > test-sexp-output.sexp.actual || true
	@diff -u test-sexp-output.sexp.expected test-sexp-output.sexp.actual
