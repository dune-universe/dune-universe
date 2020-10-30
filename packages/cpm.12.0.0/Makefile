.PHONY: build config clean edit install uninstall reinstall test

build:
	dune build @install
	dune build _build/default/src/test.exe

clean:
	dune clean

edit:
	emacs src/*.ml &

install: build
	dune uninstall
	dune install

uninstall:
	dune uninstall

# unit tests
test:
	dune build _build/default/src/test.exe
	_build/default/src/test.exe
