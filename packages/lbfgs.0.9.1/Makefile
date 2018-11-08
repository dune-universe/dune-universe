
build:
	dune build @install @runtest

install uninstall:
	dune $@

doc:
	sed -e 's/0.9.1/$(PKGVERSION)/' src/lbfgs.mli \
	  > _build/default/src/lbfgs.mli
	dune build @doc
	echo '.def { background: #f9f9de; }' >> _build/default/_doc/odoc.css

lint:
	opam lint lbfgs.opam

get-lbfgs:
	cd src && \
	test -d Lbfgsb.3.0 || (curl http://users.iems.northwestern.edu/~nocedal/Software/Lbfgsb.3.0.tar.gz | tar zx)
	dune exec config/rename_c_prims.exe

clean:
	dune clean


.PHONY: build tests install uninstall doc dist tar lint get-lbfgs clean
