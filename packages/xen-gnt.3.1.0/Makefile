
.PHONY: build clean test

build:
	dune build @install

test:
	dune runtest

install:
	dune install

uninstall:
	dune uninstall

.PHONY: docker
docker:
	docker build -t xen-gnt .

clean:
	dune clean
