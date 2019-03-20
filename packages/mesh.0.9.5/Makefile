TRIANGLE_URL = http://www.netlib.org/voronoi/triangle.zip

PACKAGES = $(basename $(wildcard *.opam))
PKGVERSION = $(shell git describe --always)
TARBALL = _build/mesh-$(PKGVERSION).tbz

all byte native:
	dune build @install

clean:
	dune clean

doc:
	dune build @doc

tests:
	dune runtest --force

submit:
	dune-release distrib --skip-build --skip-tests
#       Add the Triangle files so the tarball can easily be compiled.
	tar -C _build -xf $(TARBALL)
	mkdir _build/mesh-$(PKGVERSION)/triangle/triangle/
	cp -a triangle/triangle/triangle.* \
	  _build/mesh-$(PKGVERSION)/triangle/triangle/
	tar -C _build -jcf $(TARBALL) mesh-$(PKGVERSION)
	$(RM) -rf _build/mesh-$(PKGVERSION)/
	dune-release publish distrib
	dune-release opam pkg
	dune-release opam submit

update-triangle:
	@WGET=`which wget`;					\
	UNZIP=`which unzip`;					\
	if [ "x$$WGET" != "x" -a "x$$UNZIP" != "x" ]; then	\
	  mkdir -p triangle/triangle;				\
	  cd triangle/triangle;					\
	  $$WGET $(TRIANGLE_URL);				\
	  $$UNZIP triangle.zip;					\
	  $(RM) triangle.zip;                                   \
	else							\
	  echo "*** Please install wget and unzip.";	        \
	  exit 2;						\
	fi

.PHONY: all byte native doc tests submit update-triangle clean distclean
