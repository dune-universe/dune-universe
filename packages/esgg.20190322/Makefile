.SUFFIXES:
.PHONY: build clean test release

build:
	dune build esgg.exe

test: build
	./test/run.sh

clean:
	dune clean

release:
	./make_release.sh
