.PHONY: default build-prod build install uninstall test clean utop

default: build

build:
	dune build

build-prod:
	dune build --profile release

test:
	dune runtest -f

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

utop:
	dune utop lib
