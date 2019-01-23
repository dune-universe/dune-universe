# Makefile for developers.  Users can use Dune.
ODEPACK_URL = http://netlib.sandia.gov/odepack/
ODEPACK_FILES = opkda1.f opkda2.f opkdmain.f
ODEPACK_DIR = src/fortran

CURL = curl --insecure --retry 2 --retry-delay 2 --location --remote-name
PKGVERSION = $(shell git describe --always --dirty)

all:
	dune build @install

install uninstall:
	dune $@

test:
	dune runtest --force

doc: all
	sed -e 's/0.7/$(PKGVERSION)/' src/odepack.mli \
	  > _build/default/src/odepack.mli
	dune build @doc

# Get odepack FORTRAN codes
odepack:
	mkdir -p $(ODEPACK_DIR)
	cd $(ODEPACK_DIR) && \
	for f in $(ODEPACK_FILES); do \
	  $(CURL) $(addprefix $(ODEPACK_URL), $$f); \
	done
	if [ -z "$$CI" ]; then dune exec config/rename_c_prims.exe; \
	else ocaml str.cma config/rename_c_prims.ml; fi

clean:
	dune clean
	$(RM) iterate.dat

.PHONY: all install uninstall test odepack clean
