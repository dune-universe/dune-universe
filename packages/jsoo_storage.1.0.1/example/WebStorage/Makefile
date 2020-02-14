all:
	dune build index.html index.bc.js event.html event.bc.js

run: all
	cd ../../_build/default/example/WebStorage && python -m SimpleHTTPServer 8000
