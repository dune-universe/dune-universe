.PHONY: build install-deps clean test install uninstall doc

build:
	dune build @install

install-deps:
	opam install --deps-only ./semver2.opam

test:
	dune runtest

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

doc:
	dune build @doc

