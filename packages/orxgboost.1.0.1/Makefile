.PHONY: install uninstall reinstall test

build:
	jbuilder build @install

clean:
	jbuilder clean

edit:
	emacs src/*.ml &

install:
	jbuilder build @install
	jbuilder install

test:
	jbuilder build src/test.exe
	_build/default/src/test.exe -np `getconf _NPROCESSORS_ONLN`

uninstall:
	jbuilder uninstall

reinstall: uninstall install
