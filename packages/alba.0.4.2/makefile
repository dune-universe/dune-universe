.PHONY: build build_dune test_alba \
	doc \
	fmlib fmlib_js \
	core albalib \
	alba2 alba-node alba-web web-test

build:
	cd ocaml;          \
	make alba.native;  \
	make alba.d.byte;  \
	make alba.base

build_dune:
	dune build ocaml/alba2/alba2.exe; \
	dune build ocaml/alba1/alba.exe;  \
	dune build ocaml/alba1/alba.bc;   \
	dune runtest;                     \
	dune exec -- ocaml/alba1/alba.exe init    -work-dir library/alba.base; \
	dune exec -- ocaml/alba1/alba.exe compile -work-dir library/alba.base; \
	mv library/alba.base/.alba/*.json library/alba.base/alba-dir

test_alba:
	dune build ocaml/alba1/alba.exe;  \
	dune exec -- ocaml/alba1/alba.exe init    -work-dir library/alba.base; \
	dune exec -- ocaml/alba1/alba.exe compile -work-dir library/alba.base


doc:
	dune build @doc

doc-gh-pages: doc
	rm -rf gh-pages/ocaml/*; \
	cp -r _build/default/_doc/_html/* gh-pages/ocaml/

fmlib:
	dune build @ocaml/fmlib/basic/runtest

fmlib_js:
	dune build ocaml/fmlib/js/fmlib_js.cma

fmlib_node:
	dune build ocaml/fmlib/node/fmlib_node.cma

core: fmlib
	dune build @ocaml/core/runtest

albalib: core
	dune build @ocaml/albalib/runtest

alba2: albalib
	dune build ocaml/alba2/alba2.bc

alba-node: albalib
	dune build ocaml/alba-node/alba_node.bc.js


alba-web: albalib
	dune build ocaml/alba-web/alba_web.js

alba-web-gh-pages:
	dune build --profile=release ocaml/alba-web/alba_web.js; \
	rm -rf gh-pages/try/*; \
	cp ocaml/alba-web/*.js gh-pages/try/; \
	cp ocaml/alba-web/*.md gh-pages/try/; \
	cp ocaml/alba-web/*.html gh-pages/try/; \
	cp ocaml/alba-web/*.css gh-pages/try/



web-test: albalib
	dune build draft/web-test/test.js
