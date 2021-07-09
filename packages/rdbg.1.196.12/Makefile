

all:build

-include ./Makefile.version
build:
	dune build @install

include ./Makefile.test

.PHONY:doc
doc:
	 dune build  @doc

.PHONY:post_install
post_install: $(POST_INSTALL)

install: post_install
	dune install

uninstall:
	dune uninstall

reinstall: post_install
	dune reinstall

clean:clean_test
	dune clean
	find . -name my-rdbg-tuning.ml -exec rm {} \; 

###############################
# for developpers
-include ./Makefile.dev

###############################
# not gitted stuff
-include ./Makefile.local

