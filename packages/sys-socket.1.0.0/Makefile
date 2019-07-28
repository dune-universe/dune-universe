INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

.PHONY: build install uninstall clean doc test all

build:
	dune build @install

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

clean:
	dune clean

doc:
	dune build @doc

test:
	dune build @runtest

all: build test doc
