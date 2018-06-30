.PHONY : doc
doc :
	jbuilder build @doc
	rm -rf _doc/
	cp -r _build/default/_doc .

.PHONY : benchmark
benchmark :
	jbuilder exec benchmark/bench.exe

.PHONY : coverage
coverage : clean
	BISECT_ENABLE=YES jbuilder runtest
	bisect-ppx-report -I _build/default/ -html _coverage/ \
		`find . -name 'bisect*.out'`

.PHONY : clean
clean :
	rm -f `find . -name 'bisect*.out'`
	jbuilder clean
