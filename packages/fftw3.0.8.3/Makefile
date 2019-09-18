PKGVERSION = $(shell git describe --always --dirty)

all build byte native:
	dune build @install @examples

test runtest:
	dune runtest --force

install uninstall:
	dune $@

doc: build
	dune build @doc
	sed -e 's/0.8.3/$(PKGVERSION)/' --in-place \
	  _build/default/_doc/_html/fftw3/Fftw3/index.html

# Profiling
pnc:
#	$(MAKE) -C src -B pnc
	$(MAKE) -C examples -B pnc

lint:
	opam lint fftw3.opam

clean::
	dune clean

.PHONY: all byte native test runtest install uninstall doc pnc clean lint
