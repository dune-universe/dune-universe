.PHONY: build
build:
	dune build

.PHONY: clean
clean:
	dune clean

.PHONY: doc
doc:
	dune build @doc

VERSION=0.1.2
NAME=ocaml-snappy-$(VERSION)

.PHONY: release
release:
	git tag -a -m $(VERSION) v$(VERSION)
	git archive --prefix=$(NAME)/ v$(VERSION) | gzip > $(NAME).tar.gz
	gpg -a -b $(NAME).tar.gz
