.PHONY: all
all:
	dune build @check; make -C .. merlin
# This is enough to type-check the code in this directory,
# and is much faster than "make -C .. all".
# Note that we patch ./.merlin even if type-checking fails
# due e.g. to a type error.

.PHONY: clean test
clean test:
	make -C .. $@
