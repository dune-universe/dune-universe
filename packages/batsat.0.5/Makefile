
build:
	@dune build @install --profile=release

clean:
	@dune clean

doc:
	@dune build @doc

test:
	@dune runtest --force --no-buffer

ICNF_SOLVE=src/icnf-solve/icnf_solve.exe
icnf-solve: build
	@dune build $(ICNF_SOLVE) --profile=release
	@strip _build/default/$(ICNF_SOLVE)
	@ln -sf _build/default/$(ICNF_SOLVE) .

# must create this manually, since dune won't take it as a dependency
# see https://github.com/ocaml/dune/issues/1407
cargo-config-file:
	@mkdir -p .cargo
	@cat cargo-config > .cargo/config

CAML_LIB ?= $(shell ocamlc -where)

build-rust-stubs: cargo-config-file
	@if [ "$$(uname)" = "Darwin" ]; then \
		RUSTFLAGS='-L $(CAML_LIB) -lcamlrun' cargo build --release --frozen ; \
	else \
		cargo build --release --frozen ; \
  	fi

all: build test

VERSION=$(shell awk '/^version:/ {print $$2}' batsat.opam)
update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/*.ml) $(wildcard src/*.mli)
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/*.ml) $(wildcard src/*.mli)

reindent:
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 ocp-indent -i

watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		make build-dev ; \
	done

.PHONY: prebuild check release clean

