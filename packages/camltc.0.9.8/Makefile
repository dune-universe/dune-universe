.PHONY: test build examples clean install uninstall reinstall

build:
	dune build @install

test:
	dune build @runtest

examples:
	dune build @examples

clean:
	dune clean

install: build
	dune install

uninstall: build
	dune uninstall

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

release:
	./opam-release.sh
