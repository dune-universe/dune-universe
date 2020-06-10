SHELL := bash

# ------------------------------------------------------------------------------

# The name of the library.
THIS     := monolith

# The version number is automatically set to the current date,
# unless DATE is defined on the command line.
DATE     := $(shell /bin/date +%Y%m%d)

# The repository URL (https).
REPO     := https://gitlab.inria.fr/fpottier/$(THIS)

# The archive URL (https).
ARCHIVE  := $(REPO)/repository/$(DATE)/archive.tar.gz

# ------------------------------------------------------------------------------

# [make all] compiles just the library (in the current opam switch).

.PHONY: all
all:
	@ dune build @check

# [make clean] cleans up.

.PHONY: clean
clean:
	@ $(FIND) . -name "*~" | xargs rm -f
	@ dune clean

# [make test] runs just one demo, but there are several demos.

# afl-fuzz must be interrupted by Ctrl-C after it has found some crashes
# (or after it has run long enough).

.PHONY: test show summary min
test show summary min:
	make -C demos/working/bag $@

.PHONY: install
install:
	@ dune build @install
	@ dune install -p $(THIS)

.PHONY: uninstall
uninstall:
	@ dune build @install
	@ dune uninstall

.PHONY: reinstall
reinstall:
	@ make uninstall
	@ make install

# TEMPORARY
.PHONY: foo
foo:
	opam switch 4.09.0+afl
	eval $$(opam env) && make uninstall && make install
	opam switch 4.09.1
	eval $$(opam env) && make uninstall && make install

.PHONY: pin
pin:
	opam pin add $(THIS) . --yes

.PHONY: unpin
unpin:
	opam pin remove $(THIS) --yes

# ----------------------------------------------------------------------------

# [make headache] copies a header to every source file.

# This requires a version of headache that supports UTF-8.

HEADACHE := headache
HEADER   := $(shell pwd)/header.txt

# The find utility.
FIND     := $(shell if command -v gfind >/dev/null ; \
	            then echo gfind ; else echo find ; fi)
.PHONY: headache
headache:
	$(FIND) . -regex ".*\.ml\(i\|y\|l\)?" \
	  -exec $(HEADACHE) -h $(HEADER) "{}" ";" ; \

# ----------------------------------------------------------------------------

# [make versions] compiles and tests the library under many versions of
# OCaml, whose list is specified below.

# This requires appropriate opam switches to exist. A missing switch
# can be created like this:
#   opam switch create 4.03.0

VERSIONS := \
  4.03.0 \
  4.04.2 \
  4.05.0 \
  4.06.1 \
  4.07.1 \
  4.08.1 \
  4.09.1 \
  4.10.0 \
  ocaml-variants.4.11.0+alpha1 \

.PHONY: versions
versions:
	@(echo "(lang dune 2.0)" && \
	  for v in $(VERSIONS) ; do \
	    echo "(context (opam (switch $$v)))" ; \
	  done) > dune-workspace.versions
	@ dune build --workspace dune-workspace.versions src
	@ rm -f dune-workspace.versions

# [make handiwork] runs a command in every opam switch.

.PHONY: handiwork
handiwork:
	@ current=`opam switch show` ; \
	  for v in $(VERSIONS) ; do \
	    echo "Switching to $$v..." ; \
	    opam switch $$v && \
	    eval $$(opam env) && \
	    opam install --yes afl-persistent && dune build src ; \
	  done ; \
	  opam switch $$current

# ----------------------------------------------------------------------------

DOCDIR = _build/default/_doc/_html
DOC    = $(DOCDIR)/index.html
CSS    = $(DOCDIR)/odoc.css

.PHONY: doc
doc:
	@ dune build @doc
	@ sed -i.bak 's/font-weight: 500;/font-weight: bold;/' $(CSS) && rm -f $(CSS).bak
	@ echo "You can view the documentation by typing 'make view'".

.PHONY: view
view: doc
	@ echo Attempting to open $(DOC)...
	@ if command -v firefox > /dev/null ; then \
	  firefox $(DOC) ; \
	else \
	  open -a /Applications/Firefox.app/ $(DOC) ; \
	fi

.PHONY: export
export: doc
	ssh yquem.inria.fr rm -rf public_html/$(THIS)/doc
	scp -r $(DOCDIR) yquem.inria.fr:public_html/$(THIS)/doc

# ----------------------------------------------------------------------------

.PHONY: release
release:
# Make sure the current version can be compiled and installed.
	@ make clean
	@ make install
# Check the current package description.
	@ opam lint
# Check if this is the master branch.
	@ if [ "$$(git symbolic-ref --short HEAD)" != "master" ] ; then \
	  echo "Error: this is not the master branch." ; \
	  git branch ; \
	  exit 1 ; \
	fi
# Check if everything has been committed.
	@ if [ -n "$$(git status --porcelain)" ] ; then \
	    echo "Error: there remain uncommitted changes." ; \
	    git status ; \
	    exit 1 ; \
	  else \
	    echo "Now making a release..." ; \
	  fi
# Create a git tag.
	@ git tag -a $(DATE) -m "Release $(DATE)."
# Upload. (This automatically makes a .tar.gz archive available on gitlab.)
	@ git push
	@ git push --tags

.PHONY: publish
publish:
# Publish an opam description.
	@ opam publish -v $(DATE) $(THIS) $(ARCHIVE) .

# Once the opam package has been published, run [make export].
