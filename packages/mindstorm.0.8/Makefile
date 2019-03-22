PKGVERSION = $(shell git describe --always --dirty)
GIT_REPO = $(shell grep dev-repo mindstorm.opam \
             | sed -e 's/.*\(http[^"]*\).*/\1/')

build:
	dune build @install @tests @examples

tests: build
	dune runtest --force

install uninstall clean:
	dune $@

doc: build
	dune build @doc
	sed -e 's/0.8/$(PKGVERSION)/' --in-place \
	  _build/default/_doc/_html/mindstorm/Mindstorm/index.html

website:
	export DIR=`mktemp -d /tmp/mindstorm.XXXX` && \
	git clone -b gh-pages --depth 1 $(GIT_REPO) $$DIR && \
	cp -a web/* $$DIR && \
	cd $$DIR && git add . && git commit -m "Update website" && \
	git push origin gh-pages && \
	$(RM) -r $$DIR

.PHONY: build tests install uninstall clean doc submit \
  website website-img clean
