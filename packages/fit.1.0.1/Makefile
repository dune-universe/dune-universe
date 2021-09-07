#
# This Makefile is not called from Opam but only used for
# convenience during development
#

.PHONY: all install test clean uninstall format utop

all:
	dune build

install: all
	dune install fit

uninstall:
	dune uninstall

test:
	dune runtest

clean:
	dune clean

utop:
	dune utop

format:
	dune build --auto-promote @fmt
	opam lint
	git ls-files '**/*.[ch]' | xargs -n1 indent -nut -i8

%.mli: %.ml
	dune exec -- ocaml-print-intf $<
