.PHONY: build clean edit install uninstall reinstall test

build:
	dune build @install

clean:
	dune clean

edit:
	emacs src/*.ml &

install:
	dune build @install
	dune install

uninstall:
	dune uninstall

reinstall: uninstall install

test:
	dune build src/test.exe
	_build/default/src/test.exe -np `getconf _NPROCESSORS_ONLN`
