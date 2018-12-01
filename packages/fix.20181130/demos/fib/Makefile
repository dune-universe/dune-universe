.PHONY: all
all:
	dune build Fib.exe

.PHONY: test
test: all
	_build/default/Fib.exe

.PHONY: clean
clean:
	rm -f *~
	dune clean
