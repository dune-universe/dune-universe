
.PHONY: build clean test

build:
	dune build

test:
	dune runtest

install:
	dune install

uninstall:
	dune uninstall

xen-depends: Dockerfile build.sh
	docker build -t mirage-block-xen .

xen-build: xen-depends clean
	docker run -v $(shell pwd):/src mirage-block-xen /build.sh

clean:
	rm -rf _build
