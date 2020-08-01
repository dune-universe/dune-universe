ROOT=..

SRC=$(shell find $(ROOT) -name "*.ml" -or -name "*.mli")
DUNES=$(shell find $(ROOT) -name "dune")
JS = index.js 

all: $(JS) 

$(JS): $(SRC) $(DUNES) Makefile
	dune build index.bc.js index.bc.runtime.js
	cp $(ROOT)/_build/default/example/index.bc.js index.js
