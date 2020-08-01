OCAMLC_FLAGS = -w +a-4-40-41-42-44-45-48

SRC=$(wildcard *.ml *.mli)

all: ${SRC}
	dune build webidl.cma

clean:
	dune clean
	rm webidl.cmo

FORCE:
