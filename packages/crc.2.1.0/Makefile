.PHONY: build release install uninstall clean test doc reindent

build:
	dune build @install

release:
	dune build -p crc -j $$(getconf _NPROCESSORS_ONLN)

install:
	dune install -p crc

uninstall:
	dune uninstall -p crc

clean:
	dune clean

test:
	dune runtest

doc:
	dune build @doc -profile=release

reindent:
	ocp-indent -i **/*.ml*
