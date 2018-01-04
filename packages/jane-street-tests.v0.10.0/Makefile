
NAME=jane-street-tests

.PHONY: default
defaullt: tests

.PHONY: default
tests:
	$(MAKE) -C test-inline-tests/with-oasis-method1 test
	$(MAKE) -C test-toplevel-expect-tests test

install: $(NAME).install
	opam-installer -i --prefix "$(PREFIX)" $(NAME).install

bin.lzo: $(NAME).install
	rm -rf _install
	mkdir _install
	opam-installer -i --prefix _install $(NAME).install
	cd _install && lzop -1 -P -o ../bin.lzo `find . -type f`
	rm -rf _install
