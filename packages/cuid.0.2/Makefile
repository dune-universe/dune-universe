# .PHONY: build clean test doc

all: build

# boot should install the opam tool as well

pin:
	@ opam pin add cuid . -n --yes


vendor: pin
	@ opam install cuid --deps-only --yes
	@ opam install core --yes
	@ opam install alcotest --yes
	@ opam install re --yes
	@ opam install bisect_ppx --yes
	@ opam install ocveralls --yes

build:
	@ dune build

test: clean build
	@ dune runtest

quick-test: clean build
	@ ALCOTEST_QUICK_TESTS=1 dune runtest

doc: build
	@ dune build @doc

cleanup:
	@ rm -fv *~
	@ rm -fv lib/*~
	@ rm -fv lib_test/*~
	@ rm -fv .*.un~
	@ rm -fv lib/.*.un~
	@ rm -fv lib_test/.*.un~
	@ rm -f `find . -name 'bisect*.out'`

.PHONY: clean
clean: cleanup
	@ dune clean

install: build
	@ dune install

uninstall:
	@ dune uninstall

.PHONY: coverage
coverage: clean vendor
	@ BISECT_ENABLE=YES dune runtest
	@ bisect-ppx-report -I _build/default/ -text - `find . -name 'bisect*.out'`

.PHONY: report
report: coverage
	@ ocveralls --prefix '_build/default' `find . -name 'bisect*.out'` --send

# END
