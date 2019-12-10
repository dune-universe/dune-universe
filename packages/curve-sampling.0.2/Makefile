PKGVERSION = $(shell git describe --always)

build:
	dune build @install

test:
	$(RM) -f $(wildcard _build/default/tests/*.pdf)
	dune runtest
	dune build @latex

demo:
	dune build @demo --force

install uninstall:
	dune $@

doc:
	dune build @doc
	sed -e 's/0.2/$(PKGVERSION)/' --in-place \
	  _build/default/_doc/_html/curve-sampling/Curve_sampling/index.html

lint:
	opam lint curve-sampling.opam

clean:
	dune clean

.PHONY: build test demo install uninstall doc lint clean
