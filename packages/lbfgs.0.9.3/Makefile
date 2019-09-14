PKGVERSION = $(shell git describe --always --dirty)

build:
	dune build @install
	dune runtest --force

install uninstall:
	dune $@

doc:
	dune build @doc
	sed -e 's/0.9.3/$(PKGVERSION)/' --in-place \
	  _build/default/_doc/_html/lbfgs/Lbfgs/index.html

lint:
	opam lint lbfgs.opam

get-lbfgs:
	cd src && \
	test -d Lbfgsb.3.0 || (curl http://users.iems.northwestern.edu/~nocedal/Software/Lbfgsb.3.0.tar.gz | tar zx)
	dune exec config/rename_c_prims.exe

clean:
	dune clean


.PHONY: build tests install uninstall doc dist tar lint get-lbfgs clean
