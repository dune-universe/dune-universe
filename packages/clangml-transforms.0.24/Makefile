DUNE_INSTALL_PREFIX = _build/install/default

.PHONY : all
all : clangml-transforms.opam
	dune build

.PHONY : install
install : all
	dune build @install
	dune install

.PHONY : uninstall
uninstall :
	dune uninstall

.PHONY : tests
tests :
	dune runtest

.PHONY : clean
clean :
	dune clean

clangml-transforms.opam : dune-project
	dune build clangml-transforms.opam
