all:
	dune build @install @runtest @doc --profile release
	dune build
	rm -rf docs/* && cp -r _build/default/_doc/_html/* docs/
clean:
	rm -rf _build
runtest:
	./_build/default/test/test.exe
runstest:
	./_build/default/test/test_stream.exe
pin: 
	opam pin add nmea . -n --working-dir && opam remove nmea && opam install nmea --working-dir