.PHONY: all build test clean watch

all: build

build:
	dune build @all

test:
	build
	dune runtest

clean:
	dune clean

watch:
	dune build @all --watch
