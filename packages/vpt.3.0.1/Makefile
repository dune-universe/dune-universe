.PHONY: test

all:
	jbuilder build @install

test:
	jbuilder build test.exe
	./_build/default/test.exe

install: all
	jbuilder install

uninstall:
	ocamlfind -remove vpt

clean:
	rm -rf _build
