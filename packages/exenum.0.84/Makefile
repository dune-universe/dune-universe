.PHONY: all tests clean doc build examples

build:
	jbuilder build

all:	build examples tests doc


examples:
	jbuilder build @examples/examples

clean:
	find -L . -name "*~" -delete
	jbuilder clean
	rm -rf docs/*

tests:
	jbuilder build @test/tests

doc:	build
	ocamlfind ocamldoc -package lwt -package zarith -html -d docs _build/default/src/exenum.mli _build/default/src/lwt/lwt_tester.mli -I _build/default/src/internals/ -I _build/default/src
	cp style/style.css docs/

# I do not use odoc yet, because it does not create an index page with both exenum AND exenum.lwt. I will not write it by hand.
#
#doc:	build
#	jbuilder build @doc
#	cp -R _build/default/_doc/* docs/
