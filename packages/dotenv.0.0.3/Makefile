.PHONY: all build clean

build:
	dune build @install @tests/runtest

all: build

install:
	dune install

test:
	dune build @tests/runtest

uninstall:
	dune uninstall

clean:
	rm -rf _build *.install
	dune clean
