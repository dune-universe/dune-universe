# You can set these variables from the command line.
SPHINXOPTS    = -aE -n
SPHINXBUILD   = poetry run sphinx-build
SPHINXPROJ    = Tezos
SOURCEDIR     = .
BUILDDIR      = _build

all: html linkcheck

linkcheck:
	$(SPHINXBUILD) -b linkcheck "$(SOURCEDIR)" "$(BUILDDIR)"

html: Makefile
	$(SPHINXBUILD) -b html "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)

clean:
	@-rm -Rf "$(BUILDDIR)"
