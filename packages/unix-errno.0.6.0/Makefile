PROFILE=release

.PHONY: release build install uninstall clean test doc reindent

build:
	dune build @install --profile=$(PROFILE)

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

test:
	dune runtest

# requires odoc
doc:
	dune build @doc

reindent:
	dune build @fmt --auto-promote
