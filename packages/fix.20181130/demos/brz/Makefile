SHELL := bash

.PHONY: all
all:
	dune build Main.exe

.PHONY: test
test: all
	_build/default/Main.exe
	@ if command -v dot >/dev/null ; then \
	  for f in *.dot ; do \
	    dot -Tpdf $$f -O ; \
	  done ; \
	else \
	  echo "The program dot is required to visualize DFAs. Please install graphviz." ; \
	fi

.PHONY: clean
clean:
	rm -f *~
	dune clean
