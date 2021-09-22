.PHONY: all
all:
	dune build @install

.PHONY: test
test:
	dune build src/test/Test.exe
	./_build/default/src/test/Test.exe

.PHONY: clean
clean:
	dune clean
