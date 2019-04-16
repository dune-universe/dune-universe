.PHONY: build clean doc

OPAM_PKG_CONFIG_PATH:=$(shell opam config var lib)/pkgconfig
export OPAM_PKG_CONFIG_PATH

build:
	dune build

clean:
	dune clean

doc:
	dune build @doc

distrib:
	dune-release distrib

release:
	dune-release
