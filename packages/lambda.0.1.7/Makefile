all:
	dune build @install @runtest @doc --profile release
	rm -rf docs/* && cp -r _build/default/_doc/_html/* docs/
clean:
	rm -rf _build
i:
	./_build/default/src/interp.exe
run:
	./_build/default/examples/$(BIN).exe
runtest:
	./_build/default/test/test.exe
pin: 
	opam pin add lambda . -n --working-dir && opam remove lambda && opam install lambda --working-dir