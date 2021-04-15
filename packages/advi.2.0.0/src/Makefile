FILES = Makefile.config advi-latex-files main.exe

DUNE = dune
DUNEROOT = --root=..

all: $(FILES)

$(FILES):
	$(DUNE) build $(DUNEROOT) $@

install:
	$(DUNE) build $(DUNEROOT) @install
	$(DUNE) install $(DUNEROOT)  --display=short
