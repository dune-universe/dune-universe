
.PHONY: build clean test

# mirage-profile-xen depends on mirage-xen, which depends on
# mirage-profile, so dune won't let us build it against our
# local mirage-profile by default.
build:
	dune build -p mirage-profile @install
	dune build -p mirage-profile-xen @install
	dune build -p mirage-profile-unix @install

test:
	dune runtest

install:
	dune install

uninstall:
	dune uninstall

clean:
	rm -rf _build
