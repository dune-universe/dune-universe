all:
	jbuilder build

clean: _build/
	jbuilder clean

test: lib/* test/* all
	jbuilder runtest --debug-backtraces --verbose -p pf

lib: lib/*.ml*
	jbuilder build -p pf
