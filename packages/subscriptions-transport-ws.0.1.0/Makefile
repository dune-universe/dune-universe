.PHONY: all build  clean test # run

build:
	dune build

all: build

#run: build
#	dune exec graphql

test:
	dune runtest

install:
	dune install

uninstall:
	dune uninstall

clean:
	rm -rf _build *.install
