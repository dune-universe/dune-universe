mypkgdir=$(pkglibdir)

.DEFAULT_GOAL = all

CLANGML_CMXA = _build/default/clangml/clangml.cmxa

LIBCLANGML_STUBS_A = _build/default/clangml/libclangml_stubs.a

STUBGEN_EXE = _build/default/stubgen/stubgen.exe

mypkg_SCRIPTS = $(CLANGML_CMXA) $(LIBCLANGML_STUBS_A)

.PHONY : clangml
clangml : $(CLANGML_CMXA) $(LIBCLANGML_STUBS_A)

.PHONY : stubgen
stubgen : $(STUBGEN_EXE)

$(CLANGML_CMXA) : \
		clangml/clang.ml clangml/clang.mli \
		clangml/clang__bindings.ml clangml/clang__ast.ml
	$(DUNE) build clangml/clangml.cmxa

$(LIBCLANGML_STUBS_A) : \
		clangml/clang_stubs.c clangml/libclang_extensions.cpp \
		clangml/libclang_extensions.h clangml/stubgen.h
	$(DUNE) build clangml/libclangml_stubs.a

$(STUBGEN_EXE) : $(CLANGML_CMXA) stubgen/stubgen.ml
	$(DUNE) build stubgen/stubgen.exe

current/clang__bindings.ml : $(STUBGEN_EXE) clangml/libclang_extensions.h
	$(STUBGEN_EXE) --llvm-config $(LLVM_CONFIG) current/

current/clang__bindings.mli : current/clang__bindings.ml

current/clang_stubs.c : current/clang__bindings.ml

clean-local : clean-local-dune

.PHONY : clean-local-dune
clean-local-dune :
	dune clean

_build/default/clangml.install : \
		$(CLANGML_CMXA) $(LIBCLANGML_STUBS_A) clangml.opam
	dune build @install

install : _build/default/clangml.install
	- test -z $(INSTALL_NAME_TOOL) || \
		$(INSTALL_NAME_TOOL) -add_rpath $(LLVM_LIBDIR) \
			_build/install/default/lib/stublibs/dllclangml_stubs.so
	dune install

.mli.doc.ml :
	$(OCAMLCODOC) $<

.PHONY : tests
tests :
	dune runtest

.PHONY : doc
doc :
	dune build @doc
