INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

# Default rule
default:
	dune build @install

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

doc:
	dune build @doc

clean:
	dune clean

.PHONY: default install uninstall reinstall clean doc
