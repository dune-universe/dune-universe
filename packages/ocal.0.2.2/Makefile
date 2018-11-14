DOCDIR = _build/default/_doc/_html/

.default: build

.PHONY: build
build:
	dune build

.PHONY: clean
clean:
	dune clean

.PHONY: install
install:
	dune install

.PHONY: uninstall
uninstall:
	dune uninstall

.PHONY: test
test:
	dune runtest

.PHONY: lint
lint:
	dune build @lint
	dune-release lint

.PHONY: doc
doc:
	opam list -i --silent odoc || opam install -y odoc
	dune build @doc
	dune build @doc-private

.PHONY: read
read: doc
	open $(DOCDIR)/index.html || open $(DOCDIR)

.PHONY: release
release:
	opam list -i --silent dune-release || opam install -y dune-release
	dune-release tag
	dune-release -vv
