# name = ppx_distr_guards
# bin = $(name).native

build:
	# ocamlbuild -pkgs ocaml-migrate-parsetree $(bin)
	dune build

test:
	# ocamlbuild -pkgs ocaml-migrate-parsetree $(bin) && ocamlfind ppx_tools/rewriter ./$(bin) test.ml
	dune runtest

clean:
	# ocamlbuild -clean
	dune clean

.PHONY: build test clean
