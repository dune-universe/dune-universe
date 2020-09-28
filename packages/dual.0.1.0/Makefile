INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

.PHONY: default build install uninstall utop test clean

default: build

build:
	dune build @install @runtest

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

test:
	dune runtest -f

utop:
	dune utop lib

clean:
	dune clean
