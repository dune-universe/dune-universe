.PHONY: all
all:
	dune build HashConsDemo.exe

.PHONY: test
test: all
	_build/default/HashConsDemo.exe

.PHONY: clean
clean:
	rm -f *~
	dune clean
