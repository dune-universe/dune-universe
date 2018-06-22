.PHONY: build clean install uninstall reinstall doc test test_lz4

build:
	jbuilder build @install -j 16

clean:
	rm -rf _build doc/* *.install rwdb* *.idx src/.merlin

edit:
	emacs src/*.ml &

install: build
	jbuilder install

uninstall:
	jubilder uninstall

reinstall: uninstall install

doc:
	mkdir -p doc
	ocamldoc -html -d doc src/*.mli

test:
	\rm -f rwdb rwdb.idx
	jbuilder build _build/default/src/test.exe
	_build/default/src/test.exe

test_lz4:
	\rm -f rwdb_lz4 rwdb_lz4.idx
	jbuilder build _build/default/src/test_lz4.exe
	_build/default/src/test_lz4.exe
