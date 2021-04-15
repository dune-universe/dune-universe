#*********************************************************************#
#                                                                     #
#                           Objective Caml                            #
#                                                                     #
#            Pierre Weis, projet Cristal, INRIA Rocquencourt          #
#                                                                     #
#  Copyright 2003 Institut National de Recherche en Informatique et   #
#  en Automatique.  All rights reserved.  This file is distributed    #
#  under the terms of the GNU Library General Public License.         #
#                                                                     #
#*********************************************************************#

VERSION=0.0

OLD_VERSION=0.0
PACKAGE_VERSION_FILE=agraphics.ml
DOC_VERSION_FILE=doc/graphapg.tex

BYT_LIB = agraphics.cma
BIN_LIB = agraphics.cmxa

APGR_SOURCES = config.ml options.ml apg.ml apg_run.ml agraphics.ml
SOURCES1 = $(APGR_SOURCES:.mly=.ml)
SOURCES2 = $(SOURCES1:.mll=.ml)
BYT_OBJS = $(SOURCES2:.ml=.cmo)
BIN_OBJS = $(SOURCES2:.ml=.cmx)

APGV_SOURCES = config.ml options.ml apg.ml apg_run.ml apgv.ml
APGV_BYT_OBJS = $(APGV_SOURCES:.ml=.cmo)
APGV_BIN_OBJS = $(APGV_BYT_OBJS:.cmo=.cmx)

CAML_BYT = ocamlc -g -w A -warn-error Ae
CAML_BIN = ocamlopt -w A -warn-error Ae
CAML_DEP = ocamldep
CAML_LEX = ocamllex
CAML_YAC = ocamlyacc

WITH_BYT_GRAPHICS = graphics.cma
WITH_BIN_GRAPHICS = $(WITH_BYT_GRAPHICS:.cma=.cmxa)

WITH_BYT_AGRAPHICS = agraphics.cma
WITH_BIN_AGRAPHICS = $(WITH_BYT_AGRAPHICS:.cma=.cmxa)

all: byt bin apgv# documentation

byt: depend $(BYT_LIB)

bin: depend $(BIN_LIB)

$(BYT_LIB): $(BYT_OBJS)
	$(CAML_BYT) -a -o $(BYT_LIB) $(WITH_BYT_GRAPHICS) $(BYT_OBJS)

$(BIN_LIB): $(BIN_OBJS)
	$(CAML_BIN) -o $(BIN_LIB) $(WITH_BIN_GRAPHICS) $(BIN_OBJS)

apgv: apgv.byt apgv.bin

apgv.byt: $(APGV_BYT_OBJS)
	$(CAML_BYT) -o apgv.byt $(WITH_BYT_GRAPHICS) $(APGV_BYT_OBJS)

apgv.bin: $(APGV_BIN_OBJS)
	$(CAML_BIN) -o apgv.bin $(WITH_BIN_GRAPHICS) $(APGV_BIN_OBJS)

documentation:
	cd doc; $(MAKE) all

install: byt_install bin_install

byt_install:
	mkdir -p $(LIBDIR)/graphapg
	cp -p $(SOURCES:.ml=.mli) $(LIBDIR)/graphapg/
	cp -p $(BYT_OBJS) $(BYT_LIB) $(LIBDIR)/graphapg/

bin_install:
	mkdir -p $(LIBDIR)/graphapg
	cp -p $(SOURCES:.ml=.mli) $(LIBDIR)/graphapg/
	cp -p $(BIN_OBJS) $(BIN_LIB) $(LIBDIR)/graphapg/

# Just for $(PACKAGE) maintainers, when changing the package version number
version:
	for i in $(PACKAGE_VERSION_FILE) $(DOC_VERSION_FILE); do \
	echo $$i; \
	mv $$i $$i~; \
	sed -e '/ersion/s/$(OLD_VERSION)/$(VERSION)/' $$i~ > $$i; \
	done

# Just for $(PACKAGE) maintainers, distribution of the software.
# Need an INSTALL file in the main directory.
# Need a doc directory where the documentation has been written,
# presumably in HTML with an index.html file. This directory is copied
# onto the WEB site of the package, as mentioned in $(WEBSITEDIR)
PACKAGE=graphapg
VERSION=1.1
WEBSITEDIR=/net/pauillac/infosystems/www/$(PACKAGE)
FTPDIR=/net/pauillac/infosystems/ftp/cristal/caml-light/bazar-ocaml

distribute:
	rm -rf release
	rm -rf $(WEBSITEDIR)/*
	mkdir release
	cd release; cvs co bazar-ocaml/$(PACKAGE); \
	find . -name '.cvsignore' -print | xargs rm; \
	find . -name 'CVS' -print | xargs rm -rf; \
	cp -p bazar-ocaml/$(PACKAGE)/README bazar-ocaml/$(PACKAGE)/doc/; \
	cp -p bazar-ocaml/$(PACKAGE)/INSTALL bazar-ocaml/$(PACKAGE)/doc/; \
	cp -pr bazar-ocaml/$(PACKAGE)/doc/* $(WEBSITEDIR)/; \
	ln -s $(WEBSITEDIR)/index.html $(WEBSITEDIR)/eng.htm
	- chgrp -R caml $(WEBSITEDIR)
	- chmod -R g+w $(WEBSITEDIR)
	cd release; mv bazar-ocaml/$(PACKAGE) $(PACKAGE)-$(VERSION); \
	tar zcvf $(PACKAGE)-$(VERSION).tgz $(PACKAGE)-$(VERSION); \
	mv -f $(PACKAGE)-$(VERSION).tgz $(FTPDIR)
	rm -rf release

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(CAML_BYT) -c $<

.mli.cmi:
	$(CAML_BYT) -c $<

.ml.cmx:
	$(CAML_BIN) -c $<

.mll.cmo:
	$(CAML_LEX) $<
	$(CAML_BYT) -c $*.ml

.mll.cmx:
	$(CAML_LEX) $<
	$(CAML_BIN) -c $*.ml

.mly.cmo:
	$(CAML_YAC) $<
	$(CAML_BYT) -c $*.mli
	$(CAML_BYT) -c $*.ml

.mly.cmx:
	$(CAML_YAC) $<
	$(CAML_BIN) -c $*.mli
	$(CAML_BIN) -c $*.ml

.mly.cmi:
	$(CAML_YAC) $<
	$(CAML_BYT) -c $*.mli

.mll.ml:
	$(CAML_LEX) $<

.mly.ml:
	$(CAML_YAC) $<

clean:
	rm -f *.cm[iox] *.o *.a *~ .*~ #*#
	rm -f .depend; $(CAML_DEP) *.mli *.ml > .depend
	rm -f $(BYT_LIB) $(BIN_LIB)
	rm -f apgv.byt apgv.bin
#	cd doc; $(MAKE) clean
	cd test; $(MAKE) clean

tests:
	cd test; $(MAKE) all

depend: $(SOURCES2)
	$(CAML_DEP) *.mli *.ml > .depend

include .depend
include Makefile.config
