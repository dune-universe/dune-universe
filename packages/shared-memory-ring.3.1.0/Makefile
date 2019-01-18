.PHONY: build clean test doc install uninstall

build:
	dune build

doc:
	dune build @doc

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
