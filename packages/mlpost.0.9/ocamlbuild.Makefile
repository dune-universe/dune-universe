##########################################################################
#                                                                        #
#  Copyright (C) Johannes Kanig, Stephane Lescuyer                       #
#  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           #
#                                                                        #
#  This software is free software; you can redistribute it and/or        #
#  modify it under the terms of the GNU Library General Public           #
#  License version 2.1, with the special exception on linking            #
#  described in file LICENSE.                                            #
#                                                                        #
#  This software is distributed in the hope that it will be useful,      #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  #
#                                                                        #
##########################################################################

TESTS = handbookgraphs.cmx othergraphs.cmx tests.cmx

ifeq "$(OCAMLBEST)" "opt"
TOOL= tool.native
else
TOOL= tool.byte
OBOPTS += -byte-plugin
endif

ifeq "$(TERM)" "dumb"
OCAMLBUILD_DISPLAY= -classic-display
else
OCAMLBUILD_DISPLAY=
endif

DTYPES = -tag bin_annot

OCAMLBUILD := $(OCAMLBUILDBIN) $(OBOPTS) -no-links $(DTYPES) $(TAGS) $(OCAMLBUILD_DISPLAY) -classic-display -log "build.log"

BUILD := _build/

CMI := mlpost_options.cmi
CMA := mlpost.cma mlpost_desc_options.cma mlpost_options.cma $(CMI)
CMXA := mlpost.cmxa mlpost_desc_options.cmxa mlpost_options.cmxa
OBJ := mlpost_desc_options$(LIBEXT) mlpost_options$(LIBEXT)

#FixMe Should be done only when cairo/freetype is in the mood
EXTDLL = .so
DLL := backend/dllmlpost_ft$(EXTDLL) backend/libmlpost_ft.a


ifeq "$(OCAMLBEST)" "opt"
all:
	$(OCAMLBUILD) $(CMA) $(CMXA) $(TOOL) $(DLL)

lib:
	$(OCAMLBUILD) $(CMA) $(CMXA) $(DLL)

LIB_EXT=.cma .cmxa .cmi
else
all:
	$(OCAMLBUILD) $(CMA) $(TOOL) $(DLL)

lib:
	$(OCAMLBUILD) $(CMA) $(DLL)

LIB_EXT=.cma .cmi
endif

byte :
	$(OCAMLBUILD) $(CMA) tool.byte

opt :
	$(OCAMLBUILD) $(CMXA) tool.native

check: all $(TESTS) check-examples

tool.byte:
	$(OCAMLBUILD) tool.byte

tool.opt:
	$(OCAMLBUILD) tool.native

LOCALMLPOST:=$(BUILD)tool.native -libdir $(BUILD) -v -ps -native -ccopt "-I $(BUILD)/backend"

tests: tests.ml
	$(LOCALMLPOST) -xpdf tests.ml

testbox: testbox.ml
	$(OCAMLBUILD) testbox.native
	$(BUILD)/testbox.native
	make -C test testbox
	$(PSVIEWER) test/testbox.ps

tests.pdf: tests.ml
	$(OCAMLBUILD) tests.native
	$(BUILD)/tests.native
	make -C test tests.pdf
	$(PDFVIEWER) test/tests.pdf


tests.byte: tests.ml
	$(OCAMLBUILD) tests.byte
	$(BUILD)/tests.byte
	make -C test tests
	$(PSVIEWER) test/tests.ps

handbook.pdf : handbookgraphs.ml
	$(OCAMLBUILD) generate.cmx
	$(LOCALMLPOST) -no-magic -ccopt "generate.cmx" handbookgraphs.ml
	make -C test manual
	make -C test/manual mpost

handbook: handbook.pdf
	$(PDFVIEWER) test/testmanual.pdf

other.pdf : othergraphs.ml
	$(OCAMLBUILD) generate.cmx
	$(LOCALMLPOST) -no-magic -ccopt "generate.cmx" othergraphs.ml
	make -C test other
	make -C test/othergraphs mpost

other: other.pdf
	$(PDFVIEWER) test/othergraphs.pdf

other.byte: othergraphs.ml
	$(OCAMLBUILD) othergraphs.byte
	$(BUILD)/othergraphs.byte
	make -C test other
	make -C test/othergraphs mpost
	$(PSVIEWER) test/othergraphs.ps

.PHONY: check-examples examples
SUBDIRMLPOST:=../$(BUILD)tool.native -libdir ../$(BUILD) -v -ps -native -ccopt "-I ../$(BUILD)/backend"
MAKEEXAMPLES=$(MAKE) -C examples MLPOST='$(SUBDIRMLPOST)'

check-examples: mlpost.cma tool.opt
	$(MAKEEXAMPLES) boxes.dummy
	$(MAKEEXAMPLES) paths.dummy
	$(MAKEEXAMPLES) tree.dummy
	$(MAKEEXAMPLES) terms.dummy
	$(MAKEEXAMPLES) label.dummy
	make -C multi-examples

examples: tool.opt
	$(MAKEEXAMPLES)

examples-contrib: tool.opt
	$(MAKEEXAMPLES) contrib

examples-html: tool.opt
	$(MAKEEXAMPLES) html

# Contrib
contrib: dot-contrib lablgtk-contrib

dot-contrib : lib
	@echo "make: Entering directory \`$(shell pwd)/contrib/dot'"
	cd contrib/dot && $(OCAMLBUILDBIN) $(DTYPES) -cflags -I,$(shell pwd)/_build $(addprefix mlpost_dot,$(LIB_EXT)) && cd ../..
	ln -sf contrib/dot/_build _build_dot

ifeq "$(LABLGTK2)$(CAIROLABLGTK2)" "yesyes"
lablgtk-contrib : lib
	@echo "make: Entering directory \`$(shell pwd)/contrib/lablgtk'"
	cd contrib/lablgtk && $(OCAMLBUILDBIN) $(DTYPES) -cflags -I,$(shell pwd)/_build \
		-cflags -I,$(LABLGTK2LIB) \
		-cflags -I,$(CAIROLABLGTK2LIB) \
		$(addprefix mlpost_lablgtk,$(LIB_EXT)) && cd ../..
	ln -sf contrib/lablgtk/_build _build_lablgtk

else

lablgtk-contrib :
	@echo "lablgtk2 or cairo.lablgtk2 hasn't been found I can't make mlpost_lablgtk"
endif

clean-contrib:
	cd contrib/dot && $(OCAMLBUILDBIN) -clean && cd ../..
	cd contrib/lablgtk && $(OCAMLBUILDBIN) -clean && cd ../..

# GUI

.PHONY: gui gui/gmlpost.native gui/glexer.cmo gui/glexer.cmi

gui: gui/gmlpost.native gui/glexer.cmo

gui/gmlpost.native:
	$(OCAMLBUILD) gui/gmlpost.native

gui/gmlpost.byte:
	$(OCAMLBUILD) gui/gmlpost.byte

gui/glexer.cmo:
	$(OCAMLBUILD) gui/glexer.cmo

# building the doc
##################

.PHONY: doc
doc:
	rm -f doc
	$(OCAMLBUILD) doc/index.html
	ln -s _build/doc doc

# clean
#######

clean::
	rm -rf doc
	rm -f test.dvi test.ps *.exe
	$(OCAMLBUILD) -clean

cleaner:: clean
	make -C test clean
	make -C multi-examples clean
	make -C www clean
	make -C examples clean

dist-clean distclean:: clean
	rm -f Makefile config.cache config.log config.status META version.ml myocamlbuild.ml
