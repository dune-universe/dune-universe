MAIN := _build/default/examples/strict_lwt/example_lwt_strict.exe

VERSION := $(shell git describe)

CURRENT_DOC := _build/default/_doc
DOC_RELEASES := doc/releases
FROZEN_DOC := doc/releases/$(VERSION)
LATEST_DOC = doc/latest

default:
	jbuilder build @install

lint:
	jbuilder build @install --dev

install:
	jbuilder install $(INSTALL_ARGS)

uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	@jbuilder clean

docs:
	@jbuilder build @doc
	@cp doc/odoc.css $(CURRENT_DOC)/

doc-publish: docs
	@rm -rf $(LATEST_DOC)
	@cp -r $(CURRENT_DOC) $(LATEST_DOC)

doc-release: docs
	@rm -rf $(FROZEN_DOC)
	@mkdir -p $(DOC_RELEASES)
	@cp -r $(DOCDIR) $(FROZEN_DOC)

examples: clean
	@jbuilder build examples/strict_lwt/example_lwt_strict.exe
	@jbuilder build examples/example_strict.exe
	@jbuilder build examples/example_basic.exe


run: examples
	@$(MAIN)


.PHONY: default install uninstall reinstall clean examples
