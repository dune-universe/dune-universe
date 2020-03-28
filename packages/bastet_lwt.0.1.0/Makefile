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

.PHONY: docs
docs: clean-docs
	dune build @doc

.PHONY: copy-docs
copy-docs: docs
	cp -r _build/default/_doc/_html/** docs/

.PHONY: open-docs
open-docs: copy-docs
	xdg-open docs/index.html

.PHONY: clean-docs
clean-docs:
	rm -rf docs/**

.PHONY: test
test:
	dune runtest --no-buffer

.PHONY: watch
watch:
	dune build @all -w

.PHONY: watch-test
watch-test:
	dune runtest --no-buffer -w

.PHONY: utop
utop:
	dune utop .

.PHONY: remove-switch
remove-switch:
	opam switch remove -y .

.PHONY: dev-tools
dev-tools:
	opam install -y merlin ocamlformat utop

.PHONY: 4.06-switch
4.06-switch: remove-switch
	opam switch create -y . 4.06.1 -t -d
	make dev-tools

.PHONY: default-switch
default-switch: remove-switch
	opam switch create -y . -t -d
	make dev-tools
	eval $(opam env)
