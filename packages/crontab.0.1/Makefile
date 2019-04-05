.PHONY: all install uninstall clean doc examples check

all:
	dune build @install

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

doc:
	dune build @doc

check:
	dune build tests/check.exe
	ln -fs ../_build/default/tests/check.exe tests/run-check
	make -C tests check

examples:
	dune build examples/hello.exe
	ln -fs ../_build/default/examples/hello.exe examples/run-hello
