build:
	dune build

test:
	dune runtest


fmt:
	dune build @fmt

install:
	dune build @install
	dune install

uninstall: setup.data
	dune uninstall

.PHONY: build test install uninstall clean
