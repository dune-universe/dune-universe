DUNE=dune
DARGS=$(if $(VERBOSE),--verbose,) $(DUNE_ARGS)

clean:
	$(DUNE) clean $(DARGS)

build:
	$(DUNE) build $(DARGS)

test:
	$(DUNE) runtest $(DARGS)

format:
	$(DUNE) build @fmt --auto-promote $(DARGS)

.PHONY: clean build test format
.DEFAULT: build

