

all: build test

build:
	@dune build @install

test:
	@dune runtest --no-buffer --force

clean:
	@dune clean

doc:
	@dune build @doc

watch:
	@dune build @all -w

VERSION=$(shell awk '/^version:/ {print $$2}' sqlite3_utils.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/*.ml) $(wildcard src/*.mli)
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/*.ml) $(wildcard src/*.mli)

.PHONY: benchs tests build watch
