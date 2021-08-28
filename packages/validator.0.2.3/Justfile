# .DEFAULT_GOAL := all

# ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
# $(eval $(ARGS):;@:)

all:
	opam exec -- dune build --root . @install

deps: ## Install development dependencies
	opam install -y dune-release ocamlformat utop ocaml-lsp-server
	opam install --deps-only --with-test --with-doc -y .

create_switch: ## Create an opam switch without any dependency
	opam switch create . --no-install -y

switch: ## Create an opam switch and install development dependencies
	opam install . --deps-only --with-doc --with-test
	opam install -y dune-release ocamlformat utop ocaml-lsp-server

lock: ## Generate a lock file
	opam lock -y .

build: ## Build the project, including non installable libraries and executables
	opam exec -- dune build --root .

install: all ## Install the packages on the system
	opam exec -- dune install --root .

# start: all ## Run the produced executable
# 	opam exec -- dune exec --root . bin/main.exe $(ARGS)

# Run the unit tests
test:
	opam exec -- dune runtest --root .

clean: ## Clean build artifacts and other generated files
	opam exec -- dune clean --root .

# Generate odoc documentation
doc:
	opam exec -- dune build --root . @doc
	rm -rf ./docs
	cp -r _build/default/_doc/_html ./docs

# Open odoc documentation with default web browser
servedoc: doc
	open _build/default/_doc/_html/index.html

fmt: ## Format the codebase with ocamlformat
	opam exec -- dune build --root . --auto-promote @fmt

watch: ## Watch for the filesystem and rebuild on every change
	opam exec -- dune build --root . --watch

utop: ## Run a REPL and link with the project's libraries
	opam exec -- dune utop --root . lib -- -implicit-bindings

# Run the release script
release: all
	opam exec -- dune-release tag
	opam exec -- dune-release distrib
	opam exec -- dune-release publish distrib -y
	opam exec -- dune-release opam pkg
	opam exec -- dune-release opam submit --no-auto-open -y
