
all:build

-include ./Makefile.version
build: 
	dune build @install

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

odoc:
	 dune build  @doc

###############################
# for developpers
-include ./Makefile.dev

# not gitted stuff
-include ./Makefile.local

