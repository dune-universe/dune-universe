
all:build 

-include ./Makefile.version
-include ./Makefile.local

###################################################
# Some clutches because dune does not support camlidl
lib/polka/vector.ml: lib/polka/vector.idl
	cd lib/polka; make idl

lib/lutin/lutinUtils.ml: lib/lutin/lutinUtils.idl
	cd lib/lutin/; make

###################################################
build:
	dune build @install 

doc: doc/version.tex
	cd doc/lutin-man/ && make


install:
	dune install

uninstall:
	dune uninstall

reinstall:
	dune reinstall

clean:
	dune clean

.PHONY: test slink
test: # cpcomp
	cd examples && make test


###################################################
# for developpers
-include ./Makefile.dev
