.PHONY: build clean edit install uninstall reinstall test

build:
	dune build @install -j 16

clean:
	rm -rf _build

edit:
	emacs src/*.ml TODO commands.sh &

install: build
	dune install

uninstall:
	dune uninstall

reinstall: uninstall install

test:
	dune build src/test.exe
	_build/default/src/test.exe
