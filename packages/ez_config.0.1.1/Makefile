
.PHONY: all build-deps doc sphinx odoc view fmt fmt-check install dev-deps test
DEV_DEPS := merlin ocamlformat odoc

all: build

build:
	dune build

build-deps:
	opam install --deps-only ./ez_config.opam

sphinx:
	sphinx-build sphinx docs/sphinx

doc:
	dune build @doc
	rsync -auv --delete _build/default/_doc/_html/. docs/doc

view:
	xdg-open file://$$(pwd)/docs/index.html

fmt:
	dune build @fmt --auto-promote

fmt-check:
	dune build @fmt

install:
	dune install

opam:
	opam pin -k path .

uninstall:
	dune uninstall

dev-deps:
	opam install -y ${DEV_DEPS}

test:
	dune build @runtest
