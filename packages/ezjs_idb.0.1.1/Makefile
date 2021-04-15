all: build

build:
	@dune build src

examples:
	@dune build --profile release example
	@cp -f _build/default/example/indexedDB_example.bc.js example/indexedDB-example.js

clean:
	@dune clean
	@rm -f example/indexedDB-example.js
