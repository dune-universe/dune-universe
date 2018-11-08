build:
	dune build @install @exe

test runtest:
	dune runtest --force

install uninstall clean:
	dune $@

doc: all
	dune build @doc

lint:
	@opam lint sandbox.opam

.PHONY: build test runtest install uninstall clean doc lint
