all:
	dune build @install

doc:
	dune build @doc

clean:
	dune clean

test:
	dune runtest --force

install:
	dune install

uninstall:
	dune uninstall

ARGS=

demo_spotify:
	dune exec demo/$@.exe -- $(ARGS)

.PHONY: demo_spotify test
