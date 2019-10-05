mypkgdir=$(pkglibdir)

.DEFAULT_GOAL = all

DUNE_PREFIX = _build/default
DUNE_INSTALL_PREFIX = _build/install/default

CLANGML_CMXA = clangml/clang.cmxa

LIBCLANGML_STUBS_A = clangml/libclang_stubs.a

NORM_PREFIX = $(error NORM_PREFIX should be set)

norm_files = $(shell sed -n -e 's/^\\include{\([^}]*\)}/\1/p' $(NORM_PREFIX)/std.tex)

libraries = lift ppx show printer compare

tools = stubgen norm_extractor print_tree

# This variable is expanded for each $(library)
library_cmxa = $(library)/clangml_$(library).cmxa

# This variable is expanded for each $(tool)
tool_exe = tools/$(tool)/$(tool).exe

tools_exe = $(foreach tool,$(tools),$(tool_exe))

dune_files = \
	$(CLANGML_CMXA) \
	$(LIBCLANGML_STUBS_A) \
	$(foreach library,$(libraries),$(library_cmxa))

mypkg_SCRIPTS = $(addprefix $(DUNE_PREFIX)/, $(dune_files))

# Entry targets: all, clangml, doc, tests, install, uninstall, clean,
# and one by library and one by tool.

.PHONY : all
all : clangml clangml.opam $(libraries) $(foreach tool,$(tools),tools/$(tool))

.PHONY : clangml
clangml : $(DUNE_PREFIX)/$(CLANGML_CMXA) $(DUNE_PREFIX)/$(LIBCLANGML_STUBS_A)

clangml.opam : dune-project
	dune build clangml.opam

.PHONY : stubgen
stubgen : $(DUNE_PREFIX)/$(STUBGEN_EXE)

.PHONY : doc
doc :
	dune build @doc

.PHONY : tests
tests :
	dune runtest

.PHONY : install
install :
	dune build @install
# rpath required for dynamic link on Mac OS X
	- test -z $(INSTALL_NAME_TOOL) || \
		$(INSTALL_NAME_TOOL) -add_rpath $(LLVM_LIBDIR) \
			$(DUNE_INSTALL_PREFIX)/lib/stublibs/dllclangml_stubs.so
	dune install


.PHONY : uninstall
uninstall :
	dune uninstall

clean-local : clean-local-dune

.PHONY : clean-local-dune
clean-local-dune :
	dune clean

define foreach_library
.PHONY : ($library)
$(library) : $(DUNE_PREFIX)/$(library_cmxa)
endef
$(foreach library, $(libraries), $(eval $(foreach_library)))

define foreach_tool
.PHONY : tools/$(tool)
tools/$(tool) : $(DUNE_PREFIX)/$(tool_exe)
endef
$(foreach tool, $(tools), $(eval $(foreach_tool)))

# Delegate to dune the building of $(DUNE_PREFIX)/* files
# They are all phony targets since we want to rely on dune for dependency
# management.

define foreach_dune_file
.PHONY : $(DUNE_PREFIX)/$(dune_file)
$(DUNE_PREFIX)/$(dune_file) :
	$(DUNE) build $(dune_file)
endef
$(foreach dune_file, $(dune_files) $(tools_exe), $(eval $(foreach_dune_file)))

# Targets for bootstrapping bindings

current/clang__bindings.ml : tools/stubgen
	$(DUNE_PREFIX)/$(foreach tool,stubgen,$(tool_exe)) \
		--llvm-config $(LLVM_CONFIG) current/

current/clang__bindings.mli : current/clang__bindings.ml

current/clang_stubs.c : current/clang__bindings.ml
