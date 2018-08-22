build:
	dune build @install

clean:
	dune clean
	rm -f test/*.native

install: build
	dune install

uninstall: build
	dune uninstall

test:
	dune build @test
	@ln -sf _build/default/test/test.exe test/test.native
	@ln -sf _build/default/test/lwt_test.exe test/lwt_test.native

.PHONY: clean build install uninstall test
