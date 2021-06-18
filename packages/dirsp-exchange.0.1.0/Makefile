# Builds Sphinx documentation and the library

INSTALL_ARGS       := $(if $(PREFIX),--prefix $(PREFIX),)

DUNE_REPRODUCE_OPTS = --workspace dune-workspace.dev

PUBLISHDOCS_WORKDIR  = _build/.publishdocs
PUBLISHDOCS_OCAMLVER = 4.08.0

build:
	dune build $(DUNE_REPRODUCE_OPTS) @all

build-noautogen:
	dune build --ignore-promoted-rules @all

utop-noautogen:
	dune utop --ignore-promoted-rules

test:
	dune runtest $(DUNE_REPRODUCE_OPTS)

test-noautogen:
	dune build --ignore-promoted-rules @runtest

doc: html
	dune build @doc

install:
	dune build @install $(DUNE_REPRODUCE_OPTS) $(INSTALL_ARGS)

uninstall:
	dune uninstall $(DUNE_REPRODUCE_OPTS) $(INSTALL_ARGS)

reinstall: uninstall install

lint:
	opam lint --switch 4.08.0
	opam-dune-lint

clean:
	dune clean

# We do not publish dirsp-exchange since it is empty.
publish-docs:
	if test -n "$$(git status --porcelain)"; then echo "FATAL: The working directory must be clean! All changes have to be commited to git or removed."; exit 1; fi
	$(MAKE) clean

	echo Building OCaml documentation
	install -d _build/
	echo "(lang dune 2.8)"                                    > _build/.publishdocs-dune-workspace
	echo "(context (opam (switch $(PUBLISHDOCS_OCAMLVER))))" >> _build/.publishdocs-dune-workspace
	dune build @doc \
		--release \
		--only-packages dirsp-exchange-kbb2017,dirsp-proscript,dirsp-proscript-mirage,dirsp-ps2ocaml \
	    --workspace _build/.publishdocs-dune-workspace

	echo Building Sphinx html twice so that Sphinx cross-references work ...
	$(MAKE) html
	$(MAKE) html

	echo Cloning current git repository inside a work folder ...
	git clone --branch gh-pages "file://$$PWD/.git" $(PUBLISHDOCS_WORKDIR)/
	rsync -avp --delete --copy-links _build/html/ $(PUBLISHDOCS_WORKDIR)/docs
	rsync -avp --delete --copy-links _build/$(PUBLISHDOCS_OCAMLVER)/_doc/_html/ $(PUBLISHDOCS_WORKDIR)/docs/ocaml/
	touch $(PUBLISHDOCS_WORKDIR)/docs/.nojekyll
	cd $(PUBLISHDOCS_WORKDIR) && git add -A && git commit -m "Updated site"

	echo Trying to open a web browser so you can review the final result ...
	echo "Once you are finished the review, use '(cd $(PUBLISHDOCS_WORKDIR) && git push) && git push origin gh-pages' to publish the changes"
	wslview _build/.publishdocs/docs/index.html || open _build/.publishdocs/docs/index.html || echo "Cannot find a browser. Please review the web site at _build/.publishdocs/docs/index.html"


# You can set these variables from the command line, and also
# from the environment for the first two.
SPHINXOPTS    ?=
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = .
BUILDDIR      = _build

sphinx-help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: sphinx-help Makefile build build-noautogen doc install uninstall reinstall clean publish-docs

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
