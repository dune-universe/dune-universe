all: build

build:
	dune build @all

install:
	dune install

test:
	build
	dune runtest

doc:
	dune build @doc
	rm -r docs/quests
	cp -r _build/default/_doc/_html/quests docs/quests

clean:
	dune clean
