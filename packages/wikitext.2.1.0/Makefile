DUNE=dune

.PHONY: all install uninstall clean example retest

build:
	$(DUNE) build

doc:
	$(DUNE) build @doc

runtest:
	$(DUNE) build @runtest

wikitext.install:
	$(DUNE) build @install

install: wikitext.install
	$(DUNE) install wikitext

uninstall: wikitext.install
	$(DUNE) uninstall wikitext

js:
	$(DUNE) build test/wikitext_js.bc.js
	cp _build/default/test/wikitext_js.bc.js test/wikitext.js

dump:
	$(DUNE) build test/dumptree.exe
	_build/default/test/dumptree.exe $(FILE)

clean:
	dune clean
