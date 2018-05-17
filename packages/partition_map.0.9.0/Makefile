.PHONY: default setup clean build 08 all

default: build

build:
	jbuilder build

setup:
	opam install --deps-only ./pm.opam

clean:
	jbuilder clean

bench:
	jbuilder build src/profiling/bench.exe

compare:
	jbuilder build src/profiling/compare.exe

exec_bench:
	jbuilder exec src/profiling/bench.exe

exec_compare:
	jbuilder exec src/profiling/compare.exe

test:
	jbuilder runtest

covered_test:
	BISECT_ENABLE=YES jbuilder runtest

report:
	cd _build/default && bisect-ppx-report -html ../../ src/tests/*.out
