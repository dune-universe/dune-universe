SRCFILES = src/*.ml src/*.mli tests/*.ml sims/*.ml

OCAMLFORMAT = ocamlformat \
	--inplace \
	$(SRCFILES)

OCPINDENT = ocp-indent \
	--inplace \
	$(SRCFILES)

.PHONY: all
all :
	dune build @all

.PHONY: lib
lib :
	dune build src

.PHONY: test
test : lib
	OCAMLRUNPARAM=b dune runtest --force src/

.PHONY: sim-storage
sim-storage : lib
	dune exec --release --force --no-buffer ./sims/storage.exe > sim-storage.txt

.PHONY: sim-network
sim-network : lib
	dune exec --release --force --no-buffer ./sims/network.exe > sim-network.txt

.PHONY: sim-storage-prof
sim-storage-prof : lib
	OCAMLRUNPARA=b dune build ./sims/storage.exe
	perf record --call-graph=dwarf -- _build/default/sims/storage.exe

.PHONY: cov-desc-test
cov-desc-test : desc
	find . -name '*.coverage' | xargs rm -f
	dune runtest --instrument-with bisect_ppx --force desc/
	bisect-ppx-report html

.PHONY: cov-test
cov-test : lib
	find . -name '*.coverage' | xargs rm -f
	dune runtest --instrument-with bisect_ppx --force tests/
	bisect-ppx-report html

.PHONY: debug
debug : lib
	dune exec ./debug/main.exe

.PHONY: debug-parse
debug-parse : lib
	dune exec ./debug-parse/main.exe

.PHONY: doc
doc :
	dune build @doc

.PHONY: format
format :
	$(OCAMLFORMAT)

.PHONY : clean
clean:
	dune clean
