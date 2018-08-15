.PHONY: all tests clean doc build examples install

build:
	dune build

all:	build examples tests doc


examples:
	dune build @examples/examples
	ln -fs _build/default/examples/*.exe .

tests:
	dune build @test/tests
	ln -fs _build/default/test/*.exe .

clean:
	find -L . -name "*~" -delete
	dune clean
	rm -f *.exe
	rm -rf docs/*


#doc:	build
#	ocamlfind ocamldoc -package lwt -package zarith -html -d docs _build/default/src/exenum.mli _build/default/src/lwt/lwt_tester.mli -I _build/default/src/internals/ -I _build/default/src
#	cp style/style.css docs/

doc:	build
	dune build @doc
	rm -rf docs/*
	cp -R _build/default/_doc/_html/* docs/
	cp style/style.css docs/odoc.css

