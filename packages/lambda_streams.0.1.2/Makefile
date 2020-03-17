all: build

.PHONY: clean
clean:
	dune clean
	bsb -clean-world

.PHONY: build
build:
	dune build @all
	bsb -make-world

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

.PHONY: watch-bs
watch-bs:
	bsb -make-world -w

.PHONY: utop
utop:
	dune utop .
