MAINDIRS = tex doc 

ALLDIRS = $(MAINDIRS) test examples

SRC = src/Makefile.config src/advi-latex-files src/main.exe

DUNE = dune
DUNEROOT = --root=.

.PHONY: default 
default: $(SRC)
	for dir in $(MAINDIRS); do make -C $$dir; done

.PHONY: help
help:
	@echo 'Usage:'
	@echo '    make'
	@echo '    make install'
	@echo '    make [ uninstall | clean ]'
	@echo '    make [ doc.manual | install.manual ]'
	@echo '    make [ advi | doc | test | examples | all ]'


.PHONY: advi
advi: src/main.exe

src/%: 
	$(DUNE) build $(DUNEROOT) $@

.PHONY: test examples all
test:
	make -C test

examples:
	make -C examples

.PHONY: all
all: default test examples

CONFIG = _build/src/Makefile.config

$(CONFIG): src/Makefile.config

.PHONY: doc doc.manual
doc: $(CONFIG)
	make -C doc

doc.manual: $(CONFIG) 
	make -C doc manual

INSTALL = _build/default/advi.install

$(INSTALL):
	$(DUNE) build $(DUNEROOT) @install

.PHONY: install install.manual uinstall
install: default $(INSTALL)
	$(DUNE) install $(DUNEROOT) --display=short
	for dir in $(MAINDIRS); do make -C $$dir install; done
	@echo 
	@echo 'WARNING:'
	@echo 
	@echo "  You still need to run the command 'advi-latex-files'"
	@echo "  to install some LaTeX files needed for advanced features."
	@echo

install.manual: 
	make -C doc $@

uninstall: $(INSTALL)
	for dir in $(MAINDIRS); do make -C $$dir uninstall; done
	$(DUNE) uninstall $(DUNEROOT)

.PHONY: clean 
clean: 
	for dir in $(ALLDIRS); do make -C $$dir clean; done
	$(DUNE) clean $(DUNEROOT)
