
all: build test-dune

dev: build test

build:
	@dune build --profile=release

tip-cat:
	@dune build src/bin/tip_cat.exe
	@ln -sf _build/default/src/bin/tip_cat.exe

test: test-dune test-cat test-idempotent

test-dune:
	@dune runtest --force --no-buffer

doc:
	@dune build @doc

clean:
	@dune clean

watch:
	@dune build @all -w

BENCH_DIR ?= benchmarks/isaplanner/

test-cat: build tip-cat
	@echo testing that '`parser`' works…
	@[ -d $(BENCH_DIR) ] || (echo "expect benchmarks/ to exist" && exit 1)
	@find $(BENCH_DIR) -name  '*.smt2' -print0 | xargs -0 ./tip_cat.exe -q

#find benchmarks-zipper/ -name  '*.smt2' -print0 | xargs -0 ./tip_cat.native -q

test-idempotent: build tip-cat
	@echo testing that '`printer | parser`' works…
	@find benchmarks -name  '*.smt2' -print | while read i ; do \
	  (./tip_cat.exe "$$i" | ./tip_cat.exe -q) || exit 1; \
	  done

.PHONY: doc watch clean test all build
