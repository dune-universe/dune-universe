.PHONY: build config clean edit install uninstall reinstall tests

build:
	jbuilder build @install
	jbuilder build _build/default/src/test.exe

clean:
	jbuilder clean

edit:
	emacs src/*.ml &

install: build
	jbuilder uninstall
	jbuilder install

uninstall:
	jbuilder uninstall

# unit tests
tests:
	jbuilder build _build/default/src/test.exe
	_build/default/src/test.exe
