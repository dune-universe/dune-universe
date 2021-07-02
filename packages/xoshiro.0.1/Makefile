.PHONY: build doc test bench install uninstall clean

build:
	dune build @install

doc:
	dune build @doc
	[ -e doc ] || ln -sf _build/default/_doc/_html doc

test:
	dune test

bench:
	dune exec bench/run.exe

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
	rm -f *.opam
	rm -f doc
