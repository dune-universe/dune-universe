all:
	dune build

doc:
	dune build @doc

test:
	dune runtest

clean:
	dune clean

install:
	dune build @install
	dune install

uninstall:
	dune uninstall


%.bc.js: all
	dune build $(@) --profile release

example: example/receive.bc.js example/send.bc.js
