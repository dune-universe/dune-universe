# ------------------------------------------------------------------------------

# The name of the library.
THIS     := sek

# The version number is automatically set to the current date,
# unless DATE is defined on the command line.
DATE     := $(shell /bin/date +%Y%m%d)

# The repository URL (https).
REPO     := https://gitlab.inria.fr/fpottier/$(THIS)

# The archive URL (https).
ARCHIVE  := $(REPO)/repository/$(DATE)/archive.tar.gz

# The find utility.
FIND     := $(shell if command -v gfind >/dev/null ; \
	            then echo gfind ; else echo find ; fi)

# The default profile used by [make all].
PROFILE  := --profile dev
  # Building in dev mode should be the default, as it implies that
  # warnings are enabled (and are considered errors).
  # Building in release mode allows inspecting flambda's
  # inlining reports.
  # The benchmarks are built in release mode.

# The targets built my [make all].
TARGETS  := src monolith benchmark

# ------------------------------------------------------------------------------

# Commands.

# [make install] installs the library built in dev mode, whereas [make pin]
# installs the library build in release mode. The difference between the two
# modes is defined in src/dune.

.PHONY: all
all:
	@ dune build $(PROFILE) $(TARGETS) ; make merlin
# Regardless of whether compilation succeeds or fails,
# invoke [make merlin] to patch src/.merlin.

# [make setup] installs the packages that we need in whatever opam
# switch is currently active.

.PHONY: setup
setup:
	opam install "dune>=2.0" "cppo" "pprint>=20200410" "seq" "odoc"

.PHONY: merlin
merlin:
# Add a preprocessing directive at the end of the FLG line.
	@ sed -i.bak '/^FLG/ s/$$/ -pp "cppo -D dev"/' src/.merlin
	@ rm -f src/.merlin.bak

.PHONY: install
install:
	@ dune build @install
	@ dune install -p $(THIS)

.PHONY: clean
clean:
	@ $(FIND) . -name "*~" | xargs rm -f
	@ rm -f dune-workspace.versions
	@ dune clean

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

.PHONY: test
test:
	@ dune runtest
	make -C monolith $@

.PHONY: multicore show summary min
show summary min:
	@ make -C monolith $@

multicore:
	@ echo "[make multicore] is temporarily disabled. Use [make test] for now."

.PHONY: benchmark
benchmark:
	@ make -C benchmark $@

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

.PHONY: play
play:
	echo '#use "play.ml";;' | ocaml

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
  4.09.0+bytecode-only \
  4.10.0 \
  ocaml-variants.4.11.0+alpha3 \

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
	    opam install --yes cppo "pprint>=20200410" && make -C src ; \
	  done ; \
	  opam switch $$current

# This requires a version of headache that supports UTF-8.

HEADACHE := headache
HEADER   := $(shell pwd)/header.txt

.PHONY: headache
headache:
	for d in src monolith benchmark ; do \
	  $(FIND) $$d -regex ".*\.ml\(i\|y\|l\)?" \
	    -exec $(HEADACHE) -h $(HEADER) "{}" ";" ; \
	done

.PHONY: release
release:
# Make sure the current version can be compiled and installed.
	@ make clean
	@ make install
# Make sure play.ml and debug.ml can be executed.
	@ ocaml play.ml
	@ ocaml debug.ml
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
