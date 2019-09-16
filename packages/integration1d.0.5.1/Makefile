TESTS=$(wildcard examples/*.ml)

build:
	dune build @install $(TESTS:.ml=.exe)

tests: build
	for t in $(TESTS:.ml=.exe); do \
	  echo  "### Executing test $$t ###"; \
	  ./_build/default/$$t; \
	done

install uninstall:
	dune $@

doc:
	dune build @doc
	sed -e 's/0.5.1/$(PKGVERSION)/' --in-place \
	  _build/default/_doc/_html/integration1d/Integration1D/index.html

clean::
	dune clean

.PHONY: build tests install uninstall doc upload-doc clean
