# As suggested by Issuu:
#
# https://engineering.issuu.com/2018/11/20/our-current-ocaml-best-practices-part-1
all: build

.PHONY: clean
clean:
	dune clean

.PHONY: build
build:
	dune build @all

.PHONY: fmt
fmt:
	dune build @fmt --auto-promote

.PHONY: doc
doc:
	dune build @doc

.PHONY: open-doc
open-doc:
	open _build/default/_doc/_html/index.html

.PHONY: test
test:
	dune runtest --force
