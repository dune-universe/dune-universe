PKGVERSION = $(shell git describe --always --dirty)

TESTS=$(wildcard tests/*.ml)

build:
	dune build @install $(TESTS:.ml=.exe)

tests: build
	for t in $(TESTS:.ml=.exe); do \
	  echo "Executing test $$t"; \
	  ./_build/default/$$t; \
	done

install uninstall:
	dune $@

doc:
	dune build @doc
	sed -e 's/0.1.1/$(PKGVERSION)/' --in-place \
	  _build/default/_doc/_html/moss/Moss/index.html

.PHONY: build tests install uninstall doc
