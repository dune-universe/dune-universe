.PHONY: build release install uninstall clean test doc reindent

build:
	dune build @install

release:
	dune build --profile release @install

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

test:
	dune runtest

# requires qemu-img
benchmark: build
	./benchmark.sh

# requires odoc
doc:
	dune build @doc

gh-pages:
	bash .docgen.sh

reindent:
	ocp-indent --syntax cstruct -i lib/*.mli
	ocp-indent --syntax cstruct -i lib/*.ml
	ocp-indent --syntax cstruct -i lib_test/*.ml
	ocp-indent --syntax cstruct -i cli/*.ml
