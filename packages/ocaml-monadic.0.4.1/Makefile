.PHONY: all clean repl test

all:
	dune build

repl:
	dune utop src -- -require ocaml-monadic

test:
	dune runtest

clean:
	dune clean
