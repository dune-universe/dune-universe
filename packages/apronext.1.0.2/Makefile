default: build doc

build:
	dune build

doc:
	dune build @doc
	mkdir -p "docs"
	cp -r _build/default/_doc/_html/* docs/

test:
	@dune runtest -f

clean:
	dune clean

install:
	dune install

uninstall:
	dune uninstall

.PHONY: build test clean
