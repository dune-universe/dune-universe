PROFILE=release

.PHONY: build release install uninstall clean test doc reindent

build:
	dune build @install

release:
	dune build @install --profile=$(PROFILE)

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

test:
	dune runtest --profile=$(PROFILE)

# requires odoc
doc:
	dune build @doc --profile=$(PROFILE)

reindent:
	find lib* -name *.ml -o -name *.mli | xargs ocp-indent --syntax cstruct -i

.DEFAULT_GOAL := release
