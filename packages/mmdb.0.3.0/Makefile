.PHONY: build
build: ## Build the code
	dune build

.PHONY: clean
clean: ## Clean the source tree
	dune clean

.PHONY: format
format: ## Reformat all code
	dune build @fmt --auto-promote

.PHONY: test
test: ## Run the tests
	dune runtest --force

.PHONY: help
help: ## Display this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: release
release: ## Create a new release on Github. Prepare the release for publishing on opam repositories.
	dune-release tag
	dune-release distrib
	dune-release publish
	dune-release opam pkg
