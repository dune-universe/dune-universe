build:
	dune build @install

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

doc:
	dune build @doc

pushdoc: doc
	git checkout gh-pages
	cp -r _build/default/_doc/_html/* .
	git commit -a
	git push
	git checkout master

test:
	dune runtest --no-buffer

all: build test doc

.PHONY: build install uninstall clean doc test all pushdoc
