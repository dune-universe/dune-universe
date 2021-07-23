
all: apps man

NAME=dose3
# make sure that generated tarballs are labeled according to the state of git
VERSION=$(shell git describe --tags --always --dirty)
DIST_DIR = $(NAME)-$(VERSION)
DIST_TARBALL = $(DIST_DIR).tar.gz

apps:
	@dune build

files=$(shell find src/ \( -name "*.ml" -o -name "*.mli"  -o -name "*.mlt" \) -not -path "src/experimental/*" -type f -print)

fmt:
	ocamlformat --enable-outside-detected-project --inplace ${files}

clean:
	@dune clean
	cd doc && $(MAKE) clean

test:
	@dune runtest

# stuff not not put in a distribution tarball
DIST_EXCLUDE = cudf tests $(wildcard */tests) experimental doc/webpages

INSTALL_STUFF_ = META
INSTALL_STUFF_ += $(wildcard _build/doselibs/*.cma _build/doselibs/*.cmi)
INSTALL_STUFF_ += $(wildcard _build/doselibs/*.cmxa _build/doselibs/*.cmxs)
INSTALL_STUFF_ += $(wildcard _build/doselibs/*.a)
#INSTALL_STUFF_ += $(wildcard _build/*/*.mli)
INSTALL_STUFF_ += $(wildcard _build/rpm/*.so)

exclude_cudf = $(wildcard _build/doselibs/*cudf* _build/cudf/*)
INSTALL_STUFF = $(filter-out $(exclude_cudf), $(INSTALL_STUFF_))

installlib: META installcudf
	@test -d $(LIBDIR) || mkdir -p $(LIBDIR)
	@test -d $(LIBDIR)/stublibs || mkdir -p $(LIBDIR)/stublibs
	@$(INSTALL) -patch-version $(VERSION) $(NAME) $(INSTALL_STUFF)
	@echo "Install dose librairies to $(LIBDIR)"

install: installlib
	@cd _build/applications ; \
	install -d $(BINDIR) ; \
	for f in $$(ls *.$(OCAMLEXT)) ; do \
		install $(INSTALLOPTS) $$f $(BINDIR)/$${f%.$(OCAMLEXT)}$(EXE) ; \
	done
	@$(LN) $(BINDIR)/distcheck $(BINDIR)/debcheck$(EXE)
	@$(LN) $(BINDIR)/distcheck $(BINDIR)/rpmcheck$(EXE)
	@$(LN) $(BINDIR)/distcheck $(BINDIR)/eclipsecheck$(EXE)
	@echo "Install dose binaries to $(BINDIR)"

uninstalllib:
	@$(OCAMLFIND) remove -destdir $(LIBDIR) $(NAME)
	@echo "Uninstall dose librairies from $(LIBDIR)"

uninstall: uninstalllib uninstallcudf
	@for f in $$(ls *.$(OCAMLEXT)) ; do \
	  rm -f $(BINDIR)/$${f%.$(OCAMLEXT)}$(EXE) ; \
	done
	@rm -f $(BINDIR)/debcheck$(EXE) $(BINDIR)/rpmcheck$(EXE) $(BINDIR)/eclipsecheck$(EXE)
	@echo "Uninstall dose binaries from $(BINDIR)"

dist: ./$(DIST_TARBALL)
./$(DIST_TARBALL):
	@if [ -d ./$(DIST_DIR)/ ] ; then rm -rf ./$(DIST_DIR)/ ; fi
	@if [ -d ./$(DIST_TARBALL) ] ; then rm -f ./$(DIST_TARBALL) ; fi
	@mkdir ./$(DIST_DIR)/ ; git archive --format=tar HEAD | tar -x -C ./$(DIST_DIR)/
	@for f in $(DIST_EXCLUDE) ; do rm -rf ./$(DIST_DIR)/$$f; done
	@tar czf ./$(DIST_TARBALL) ./$(DIST_DIR)
	@rm -rf ./$(DIST_DIR)
	@echo "Distribution tarball: ./$(DIST_TARBALL)"

changelog:
	dch -c CHANGES --package $(NAME) -v $(VERSION)

credits:
	@git log --pretty=format:'%aN        %aE' | LC_ALL=C sort -u | awk -F'\t' '{printf("\t%s <%s>\n",$$1,$$2)}';

doc:
	dune build @doc
	$(MAKE) -C doc all

man:
	$(MAKE) -C doc/manpages

.PHONY: \
	common algo pef versioning debian rpm csw doseparseNoRpm doseparse \
	all clean top-level headers test tags install uninstall dist doc man
