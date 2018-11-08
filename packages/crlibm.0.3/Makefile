PKGVERSION = $(shell git describe --always)
PKGTARBALL = crlibm-$(PKGVERSION).tbz

all build:
	dune build @install @runtest

speed:
	dune build tests/speed.exe
#	Execute directly to see the output progress:
	_build/default/tests/speed.exe

install uninstall:
	dune $@

doc:
	sed -e 's/0.3/$(PKGVERSION)/' src/crlibm.mli \
	  > _build/default/src/crlibm.mli
	dune build @doc
	@echo '.def { background: #f0f0f0; }' \
	  >> _build/default/_doc/_html/odoc.css

get-crlibm:
	git subtree add -P src/crlibm \
	  https://scm.gforge.inria.fr/anonscm/git/metalibm/crlibm.git \
	  master

clean:
	dune clean

lint:
	opam lint crlibm.opam

.PHONY: all build speed install uninstall doc get-crlibm clean lint
