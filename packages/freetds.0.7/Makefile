all: build

build:
	@dune build @install @test

clean:
	@dune clean
	@$(RM) **/.merlin

install uninstall:
	@dune $@

test:
	@dune runtest --force

doc:
	@dune build @doc

.PHONY: all build clean install uninstall test doc
