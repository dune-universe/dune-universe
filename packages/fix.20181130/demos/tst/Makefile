.PHONY: all
all:
	dune build Test.exe

.PHONY: test
test: all
	_build/default/Test.exe

.PHONY: clean
clean:
	rm -f *~
	dune clean
