default: clean build

run: clean build
	./_build/default/bin/lilacbin.exe $(args)

clean:
	dune clean

build:
	dune build

test: clean build
	dune runtest

coverage: clean build
	find . -name '*.coverage' | xargs rm -f
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report summary

.PHONY: clean
