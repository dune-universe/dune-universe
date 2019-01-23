.PHONY : all
all:
	dune build ocamlcodoc/ocamlcodoc.exe

.PHONY : install
install :
	dune build ocamlcodoc @install
	dune install


.PHONY : tests
tests :
	dune runtest
