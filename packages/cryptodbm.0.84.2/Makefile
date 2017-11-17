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
	ocamlfind ocamldoc -html -d docs _build/default/src/cryptodbm.mli
	cp style/style.css docs/

# I do not use odoc yet, because it is undocumented, and the generated files use the css file in ../../odoc.css
# which is not correct when deploying the pages. I will not /sed/ the output files to fix this.
