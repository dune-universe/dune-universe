.PHONY: default
default: dev

.PHONY: dev
dev:
	dune build @all --profile dev

.PHONY: test
test:
	dune runtest

.PHONY: clean
clean:
	dune clean
