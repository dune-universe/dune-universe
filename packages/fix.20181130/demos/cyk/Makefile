.PHONY: all
all:
	dune build Bench.exe

.PHONY: test
test: all
	_build/default/Bench.exe

.PHONY: clean
clean:
	rm -f *~
	dune clean
