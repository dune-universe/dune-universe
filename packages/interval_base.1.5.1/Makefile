PACKAGES = $(basename $(wildcard *.opam))
PKGVERSION = $(shell git describe --always)

all build:
	dune build @install @examples
	dune build @runtest --force

ocamlfpu: all
	cd _build/default/src/ && ocamlmktop -I . -o ocamlfpu interval.cma

install uninstall:
	dune $@

tests: all
	@ echo "Run tests..."
	@ cd _build/default/tests/ && ./tests.exe > tests.log && \
	C=`grep DEFINED tests.log | wc -l` && \
	if [ $$C -gt 0 ]; then \
	  echo "Tests: $$C errors (see _build/default/tests/tests.log)"; \
	  grep -B 3 -A 2 DEFINED tests.log; \
	else echo "All tests passed successfully"; fi

test-speed:
	dune exec tests/tests_speed.exe

graphs:
	dune build @graphs
	@echo See $(wildcard _build/default/tests/*.pdf)

clean:
	dune clean

doc:
	dune build @doc
	sed -e 's/1.5.1/$(PKGVERSION)/' --in-place \
	  _build/default/_doc/_html/interval_base/Interval/index.html \
	  _build/default/_doc/_html/interval_intel/Interval_intel/index.html \
	  _build/default/_doc/_html/interval_crlibm/Interval_crlibm/index.html

pin:
#	Installation in the right order
	opam pin -k path -y add interval_base.dev .
	opam pin -k path -y add interval_intel.dev .
	opam pin -k path -y add interval_crlibm.dev .
	opam pin -k path -y add interval.dev .

unpin:
	opam pin remove -y $(PACKAGES)

upgrade:
	opam upgrade -y $(PACKAGES)

lint:
	opam lint

.PHONY: all build ocamlfpu install uninstall tests test-speed graphs \
  examples clean doc bistro pin unpin upgrade lint
