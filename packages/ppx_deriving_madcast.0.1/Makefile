build:
	jbuilder build @install

doc:
	jbuilder build @doc

install:
	jbuilder install

uninstall:
	jbuilder uninstall

release:
	sh release.sh

test: build
	jbuilder build .ppx/ppx_deriving_madcast/ppx.exe
	make -C test

clean:
	jbuilder clean
	rm -rf ppx_deriving_madcast.[0-9.]*
	make -C test clean

.PHONY: build doc install uninstall release test clean
