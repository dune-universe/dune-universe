BROWSER = firefox
BENCH_D = bench
DOCS_D = docs
TEST_COV_D = /tmp/bio_io

.PHONY: bench
bench: bench_fasta bench_fasta_exe

.PHONY: bench_fasta
bench_fasta:
	dune build --profile=release && \
	  BENCH_FASTA_INFILE=$(BENCH_D)/seqs.10000.faa \
	  dune exec ./bench/bench_fasta.exe 2> /dev/null

.PHONY: bench_fasta_exe
bench_fasta_exe:
	dune build --profile=release && \
	  BENCH_FASTA_INFILE=$(BENCH_D)/seqs.10000.faa \
	  hyperfine -w 30 './_build/default/bench/bench_fasta_exe.exe 2> /dev/null'

.PHONY: build
build:
	dune build --profile=release

.PHONY: check
check:
	dune build @check

.PHONY: clean
clean:
	dune clean

.PHONY: docs
docs:
	dune build @doc
	$(BROWSER) _build/default/_doc/_html/index.html

.PHONY: docs_site
docs_site:
	if [ -d $(DOCS_D) ]; then rm -rf $(DOCS_D); fi
	dune build @doc && \
	  mv _build/default/_doc/_html docs && \
	  chmod 755 $(DOCS_D) && \
	  $(BROWSER) ./$(DOCS_D)/index.html

.PHONY: everything
everything: clean build test_coverage_open docs install
	@echo "EVERYTHING!!!"

.PHONY: install
install: build
	dune install

.PHONY: promote
promote:
	dune promote

.PHONY: uninstall
uninstall:
	dune uninstall

.PHONY: test
test:
	dune runtest

.PHONY: test_coverage
test_coverage:
	if [ -d $(TEST_COV_D) ]; then rm -r $(TEST_COV_D); fi
	mkdir -p $(TEST_COV_D)
	BISECT_FILE=$(TEST_COV_D)/bio_io dune runtest --no-print-directory \
	  --instrument-with bisect_ppx --force
	bisect-ppx-report html --coverage-path $(TEST_COV_D)
	bisect-ppx-report summary --coverage-path $(TEST_COV_D)

.PHONY: test_coverage_open
test_coverage_open: test_coverage
	$(BROWSER) _coverage/index.html

.PHONY: send_coverage
send_coverage: test_coverage
	bisect-ppx-report send-to Coveralls --coverage-path $(TEST_COV_D)
