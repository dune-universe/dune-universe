BROWSER = firefox
DOCS_D = docs
TEST_COV_D = /tmp/bio_io

.PHONY: build
build:
	dune build

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
	if [ -d $(DOCS_D) ]; then rm -r $(DOCS_D); fi
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
