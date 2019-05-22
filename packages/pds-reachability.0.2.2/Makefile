.PHONY: all clean repl test

all:
	dune build

repl:
	dune utop src -- -require pds-reachability

test:
	dune runtest

clean:
	dune clean
