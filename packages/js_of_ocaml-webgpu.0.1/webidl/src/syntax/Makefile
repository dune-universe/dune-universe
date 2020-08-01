OCAMLOPT_FLAGS = -w +a-4-40-41-42-44-45-48-33
OCAMLC_FLAGS = -w +a-4-40-41-42-44-45-48-33

SRC=$(wildcard *.ml *.mli)

all: ${SRC}
	dune build webidl_syntax.cma

clean:
	dune clean
	rm webidl_syntax.cmo

FORCE:
