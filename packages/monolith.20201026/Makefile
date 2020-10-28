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

# We assume that all of the demos (which also serve as tests)
# exist at depth two under demos/. The following command lists them.

DEMOS     = $(FIND) demos -mindepth 2 -maxdepth 2 -type d

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
	@ $(DEMOS) | xargs -n1 make clean --no-print-directory -C

# ------------------------------------------------------------------------------

# [make test] runs all demos in unattended mode.

# We can use either xargs or GNU parallel,
# so as to run multiple tests either sequentially or in parallel.

# The number of jobs in parallel mode must be limited,
# otherwise afl-fuzz appears to fail.

# Every demo is expected to inhabit a subsubdirectory of demos.

# Naively running [make unattended] in every demo directory in parallel does
# not work, because this causes dune to be run in every demo directory in
# parallel, and dune apparently does not allow this. To work around this
# issue, we first compile everything using one [make all] invocation at the
# toplevel, then we run [make unattended] in every demo directory while
# ignoring the dependency on [all].

# LOOP := xargs -n1
LOOP := parallel --no-notice --jobs 4

.PHONY: test
test: dependencies
	@ make -f Makefile.monolith all --no-print-directory
	@ $(DEMOS) \
	  | $(LOOP) make unattended -o all --no-print-directory -C

# [make dependencies] installs the libraries required by the demos.

# It seems that [dune external-lib-deps --missing] can produce a
# nonzero exit code. It then prints messages on stderr, including a
# suggested invocation of "opam install". Also, it does not support
# the [--switch] option. It is a real pain.

SWITCH = $(shell make -f Makefile.monolith switch)

.PHONY: dependencies
dependencies:
	@ current=`opam switch show` ; \
	for i in $(SWITCH) $$current ; do \
	  echo "Switching to $$i..." ; \
	  opam switch $$i ; \
	  if dune external-lib-deps . --display=short --missing >/dev/null 2>&1 ; then \
	    echo "No dependencies are missing in this switch." ; \
	  else \
	    hint=`(dune external-lib-deps . --display=short --missing 2>&1 || true) | grep "opam install"` ; \
	    echo "Installing the missing dependencies..." ; \
	    echo "$$hint" ; \
	    $$hint ; \
	  fi \
	done

# ------------------------------------------------------------------------------

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

.PHONY: pin
pin:
	opam pin add $(THIS) . --yes

.PHONY: unpin
unpin:
	opam pin remove $(THIS) --yes

.PHONY: repin
repin:
	@make unpin
	@make pin

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
  4.11.1 \

.PHONY: versions
versions:
	@(echo "(lang dune 2.0)" && \
	  for v in $(VERSIONS) ; do \
	    echo "(context (opam (switch $$v)))" ; \
	  done) > dune-workspace.versions
	@ dune build --workspace dune-workspace.versions src ; \
	  result=$$? ; \
	  rm -f dune-workspace.versions ; \
	  exit $$result

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
