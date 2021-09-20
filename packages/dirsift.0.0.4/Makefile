SRCFILES = src/*.ml # src/*.mli

OCAMLFORMAT = ocamlformat \
	--inplace \
	$(SRCFILES)

OCPINDENT = ocp-indent \
	--inplace \
	$(SRCFILES)

.PHONY: all
all :
	dune build @all

.PHONY: release-static
release-static :
	OCAMLPARAM='_,ccopt=-static' dune build --release src/dirsift.exe

.PHONY: doc
doc :
	dune build @doc

.PHONY: format
format :
	$(OCAMLFORMAT)

.PHONY : clean
clean:
	dune clean
