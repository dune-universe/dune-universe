.PHONY : all
all : _build/redirect/redirect.cmxa redirect.opam

_build/redirect/redirect.cmxa :
	dune build redirect/redirect.cmxa

redirect.opam : dune-project
	dune build redirect.opam

.PHONY : install
install :
	dune build @install
	dune install
