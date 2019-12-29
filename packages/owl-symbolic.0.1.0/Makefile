.PHONY: all
all: build

.PHONY: depend depends
depend depends:
	dune external-lib-deps --missing @install @runtest

.PHONY: build
build: depends
	dune build @install

.PHONY: test
test: depends
	dune runtest -j 1 --no-buffer -p owl-symbolic

.PHONY: clean
clean:
	dune clean

.PHONY: cleanall
cleanall:
	dune uninstall
	dune clean

.PHONY: install
install: build
	dune install

.PHONY: uninstall
uninstall:
	dune uninstall

.PHONY: doc
doc:
	opam install -y odoc
	dune build @doc
	cp -r _build/default/_doc/_html/* docs
	git add docs/*

.PHONY: format
format:
	dune build @fmt --auto-promote

.PHONY: example
example:
	dune build @example/all 

loc:
	cloc src/

push:
	git commit -am "coding symbolic ..." && \
	git push origin `git branch | grep \* | cut -d ' ' -f2`
