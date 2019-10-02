.PHONY : all
all: ocamlcodoc.opam $(DUNE_PREFIX)/ocamlcodoc/ocamlcodoc.exe

.PHONY : install
install :
	dune build ocamlcodoc @install
	dune install


.PHONY : tests
tests :
	dune runtest

ocamlcodoc.opam : dune-project
	dune build ocamlcodoc.opam

# .PHONY because we want dune to solve dependencies

.PHONY : $(DUNE_PREFIX)/ocamlcodoc/ocamlcodoc.exe
$(DUNE_PREFIX)/ocamlcodoc/ocamlcodoc.exe:
	dune build ocamlcodoc/ocamlcodoc.exe
