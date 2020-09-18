INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

.PHONY: build doc install uninstall utop test clean

build:
	dune build @install

doc:
	dune build @doc

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

utop:
	dune utop src/lib

test: build
	cp ./_build/default/src/bin/main.exe ./tests/l2l
	$(MAKE) -C tests

clean:
	dune clean
	$(MAKE) -C tests clean
