
.PHONY: build clean test install uninstall doc

build:
	dune build

doc:
	dune build @doc

test:
	dune runtest

install:
	dune install

uninstall:
	dune uninstall

clean:
	rm -rf _build *.install
