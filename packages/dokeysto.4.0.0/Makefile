.PHONY: build clean install uninstall reinstall doc test test_lz4 test_camltc

build:
	dune build @install -j 16

clean:
	rm -rf _build doc/* *.install rwdb* *.idx src/.merlin

edit:
	emacs src/*.ml &

install: build
	dune install

uninstall:
	dune uninstall

reinstall: uninstall install

doc:
	mkdir -p doc
	ocamldoc -html -d doc src/*.mli

test:
	\rm -f rwdb rwdb.idx
	dune build -p dokeysto _build/default/src/test.exe
	_build/default/src/test.exe

test_lz4:
	\rm -f rwdb_lz4 rwdb_lz4.idx
	dune build -p dokeysto_lz4 _build/default/src/test_lz4.exe
	_build/default/src/test_lz4.exe

test_camltc:
	\rm -f rwdb_camltc rwdb_camltc.idx
	dune build -p dokeysto_camltc _build/default/src/test_camltc.exe
	_build/default/src/test_camltc.exe
