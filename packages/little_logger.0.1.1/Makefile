BROWSER = firefox
NAME=little_logger
DOCS_D = docs
TEST_COV_D = /tmp/little_logger

.PHONY: docs
docs:
	dune build @doc

.PHONY: docs_open
docs_open: docs
	$(BROWSER) _build/default/_doc/_html/index.html

.PHONY: docs_site
docs_site:
	if [ -d $(DOCS_D) ]; then rm -rf $(DOCS_D); fi
	dune build @doc && \
	  mv _build/default/_doc/_html docs && \
	  chmod 755 $(DOCS_D) && \
	  $(BROWSER) ./$(DOCS_D)/index.html

.PHONY: test
test:
	dune runtest

.PHONY: test_coverage
test_coverage:
	if [ -d $(TEST_COV_D) ]; then rm -r $(TEST_COV_D); fi
	mkdir -p $(TEST_COV_D)
	BISECT_FILE=$(TEST_COV_D)/$(NAME) dune runtest --no-print-directory \
	  --instrument-with bisect_ppx --force
	bisect-ppx-report html --coverage-path $(TEST_COV_D)
	bisect-ppx-report summary --coverage-path $(TEST_COV_D)

.PHONY: test_coverage_open
test_coverage_open: test_coverage
	$(BROWSER) _coverage/index.html

.PHONY: send_coverage
send_coverage: test_coverage
	bisect-ppx-report send-to Coveralls --coverage-path $(TEST_COV_D)

.PHONY: build
build:
	dune build --profile=release

.PHONY: install
install: build
	dune install
