default: build doc

build:
	dune build

test:
	@dune runtest -f

doc:
	dune build @doc
	mkdir -p "docs"
	cp -r _build/default/_doc/_html/* docs/

clean:
	dune clean

install:
	dune install picasso

uninstall:
	dune uninstall picasso

.PHONY: build test clean
