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
test: build
test: ## Run the tests
	dune runtest --force

.PHONY: release
release: ## Create a new release on Github and prepare for publishing on opam repositories
	dune-release tag
	dune-release distrib
	dune-release publish
	dune-release opam pkg

.PHONY: generate-embedded
generate-embedded: build
generate-embedded: ## Generates code for Protocol Buffer definition files shipped along with protoc
	$(eval PROTOBUF_INCLUDE := $(shell find /usr -type d -path '*include/google/protobuf' 2>/dev/null | head -n 1 | xargs dirname | xargs dirname))
	@find $(PROTOBUF_INCLUDE) -iname '*.proto' | WITH_DERIVERS="eq,show" xargs protoc \
		-I $(PROTOBUF_INCLUDE) \
		--plugin=protoc-gen-ocaml=_build/default/src/protocell/protocell.exe \
		--ocaml_out=src/protocell_google

.PHONY: help
help: ## Display this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
