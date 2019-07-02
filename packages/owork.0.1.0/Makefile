.PHONY: build
build:
	dune build
	dune build exe/main.exe

.PHONY: check
check:
	dune build @check

.PHONY: test
test:
	dune runtest

.PHONY: install
install: build
	dune build @install
	dune install

.PHONY: uninstall
uninstall:
	dune uninstall

.PHONY: distrib
distrib:
	dune-release tag
	dune-release distrib

.PHONY: publish
publish:
	dune-release publish
	dune-release opam pkg
	dune-release opam submit

.PHONY: release
release: distrib publish

.PHONY: promote
promote:
	dune promote

.PHONY: coverage
coverage:
	BISECT_ENABLE=yes dune runtest --force
	bisect-ppx-report -I _build/default/ -html coverage/ `find . -name 'bisect*.out'`

.PHONY: clean
clean:
	dune clean

.PHONY: format
format:
	dune build @fmt --auto-promote
