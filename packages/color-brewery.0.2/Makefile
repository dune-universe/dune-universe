PKGVERSION = $(shell git describe --always)

build:
	ulimit -s 16384 && dune build @install @exe

install uninstall:
	dune $@

test:
	ulimit -s 16384 && dune runtest --force

distrib:
	dune-release tag
	ulimit -s 16384 && dune-release distrib

doc: build
	dune build @doc
	@sed -e 's/0.2/$(PKGVERSION)/' --in-place \
	  _build/default/_doc/_html/color-brewery/Color_brewery/index.html
	@echo "See _build/default/_doc/_html/index.html"

lint:
	@opam lint

# See the beginning of tools/make_brewer.ml for the JSON files to download.
# This only need to be run if these JSON files change — the result is
# commited so the rest of the code is independent of this.
preconfigure:
	dune exec tools/make_palettes.exe

clean:
	dune clean

.PHONY: build install uninstall test distrib doc lint preconfigure clean
