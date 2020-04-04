SHELL := bash

.PHONY: all
all:
	@ dune build --profile release .

.PHONY: clean
clean:
	@ dune clean

.PHONY: benchmark
benchmark:
	@ dune exec --profile release -- src/Main.exe
