.PHONY : all
all : lib doc

.PHONY : lib
lib :
	dune build

.PHONY : test
test :
	dune runtest

.PHONY : doc
doc :
	dune build @doc
	rm -rf _doc/
	cp -r _build/default/_doc .

.PHONY : benchmark
benchmark :
	dune exec benchmarks/bench.exe

.PHONY : coverage
coverage : clean
	BISECT_ENABLE=YES dune runtest
	bisect-ppx-report -I _build/default/ -html _coverage/ \
		`find . -name 'bisect*.out'`

.PHONY : profile
profile : clean
	dune exec profile/bench.exe
	gprof _build/default/profile/bench.exe gmon.out > profile.txt

.PHONY : clean
clean :
	rm -f `find . -name 'bisect*.out'`
	dune clean
