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
	$(OCPINDENT)

.PHONY: gen
gen :
	cd gen/ && dune build gen_time_zone_data.exe
	dune exec gen/gen_time_zone_data.exe

.PHONY : clean
clean:
	dune clean
