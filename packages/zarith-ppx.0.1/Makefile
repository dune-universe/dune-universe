PKG=zarith-ppx

all: build test

build:
	@dune build @install

test:
	@dune runtest --no-buffer --force

clean:
	@dune clean

doc:
	@dune build @doc

VERSION=$(shell awk '/^version:/ {print $$2}' '$(PKG).opam')

REPO=git@github.com:Drup/$(PKG)
DOCDIR=.gh-pages

$(DOCDIR)/.git:
	mkdir -p $(DOCDIR)
	cd $(DOCDIR) && (\
		git clone -b gh-pages $(REPO).git . \
	)

gh-pages: $(DOCDIR)/.git doc
	git -C $(DOCDIR) pull
	cp -r _build/default/_doc/_html/* $(DOCDIR)/doc/dev/
	git -C $(DOCDIR) add --all
	git -C $(DOCDIR) commit -a -m "gh-page updates"
	git -C $(DOCDIR) push origin gh-pages

.PHONY: doc test gh-pages
