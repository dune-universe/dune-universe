.PHONY: all build clean

build:
	esy b dune build @install

all: build

install:
	esy b dune install

uninstall:
	esy b dune uninstall

clean:
	rm -rf _build *.install
	esy b dune clean
