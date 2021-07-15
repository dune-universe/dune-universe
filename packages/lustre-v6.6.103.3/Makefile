
all:build man
build:
	dune build @install

-include ./Makefile.version


install:
	dune install

uninstall:
	dune uninstall

reinstall:
	dune install

clean:
	dune clean

man:
	cd lv6-ref-man && make || echo "*** ref manual building failed"


###############################
# for developpers
-include ./Makefile.dev

# not gitted stuff
-include ./Makefile.local

