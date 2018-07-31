.PHONY: clean build utop async lwt

all: build

clean:
	-rm -rf _build
	-rm -rf src/.merlin
	-rm -rf *.install

build:
	dune build --only-packages=tube @install

utop:
	dune exec utop
