build:
	dune build @install

clean:
	dune clean
	rm -f ./test_it.native

install: build
	dune install

uninstall: build
	dune uninstall

examples:
	dune build @examples
	@ln -sf _build/default/examples/test_it.exe ./test_it.native

.PHONY: clean build install uninstall examples
