.PHONY: build doc test install uninstall clean

build:
	dune build @install
	[ -e bin ] || ln -sf _build/install/default/bin bin
	[ -e lib ] || ln -sf _build/install/default/lib lib

doc:
	rm -Rf doc/*
	dune build @doc
	[ -e doc ] || ln -sf _build/default/_doc/_html doc
	mv -f doc/*/* doc/
	sed -i "s%\.\./\(odoc\.css\|highlight\.pack\.js\)%./\1%g" $$(find doc/ -name '*.html')
	sed -i "s%<title>[^.]*\.\(.*\))</title>%<title>\1</title>%" $$(find doc/ -name '*.html')
	sed -i "s%<title>.*</title>%<title>OCaml Bindings for TestU01</title>%" doc/index.html
	sed -i "s%\(<nav>.* – <[^<>]*>\)\([^<>]*\)\(</a> &#x00BB;\)%\1Index\3%" $$(find doc/ -name '*.html')
	sed -i "s%<nav>.*</nav><h1%<h1%" doc/index.html

test:
	dune test

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
	rm -f *.opam
	rm -f bin lib doc
