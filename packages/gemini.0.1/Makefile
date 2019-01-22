
build:
	dune build

install: build
	dune install

default:
	build

clean :
	dune clean

test: build
	dune build @runtest

.PHONY:
	clean test build
