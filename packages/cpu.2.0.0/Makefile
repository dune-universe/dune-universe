.PHONY: build clean edit install uninstall reinstall

build:
	autoconf
	autoheader
	./configure
	dune build @install -j `getconf _NPROCESSORS_ONLN`

clean:
	dune clean
	rm -rf autom4te.cache/ configure config.log config.status \
		src/config.h src/config.h.in

edit:
	emacs src/*.ml &

install: build
	dune install

uninstall:
	dune uninstall

reinstall: uninstall install
