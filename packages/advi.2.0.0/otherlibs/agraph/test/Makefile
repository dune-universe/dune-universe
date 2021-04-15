#*********************************************************************#
#                                                                     #
#                           Objective Caml                            #
#                                                                     #
#            Pierre Weis, projet Cristal, INRIA Rocquencourt          #
#                                                                     #
#  Copyright 2000 Institut National de Recherche en Informatique et   #
#  en Automatique.  All rights reserved.  This file is distributed    #
#  under the terms of the GNU Library General Public License.         #
#                                                                     #
#*********************************************************************#

# $Id$

# Files that generate encapsulated PostScript
<<<<<<< Makefile
EPS_SOURCES = essai1.ml essai2.ml essai3.ml \
=======
EPSSOURCES = essai1.ml essai2.ml essai3.ml essai4.ml essai5.ml essai6.ml \
>>>>>>> 1.15
euro.ml graphps_logo.ml titi5.ml titi8.ml
# Files that generate PostScript
PS_SOURCES = uros.ml

SOURCES = $(EPS_SOURCES) $(PS_SOURCES)

EPS_PRG = $(EPS_SOURCES:.ml=)
PS_PRG = $(PS_SOURCES:.ml=)
BYT_EXECS = $(EPS_SOURCES:.ml=.byt) $(PS_SOURCES:.ml=.byt)
BIN_EXECS = $(EPS_SOURCES:.ml=.bin) $(PS_SOURCES:.ml=.bin)

CAML_BYT = ocamlc -warn-error A -g -I ..
CAML_BIN = ocamlopt -warn-error A -inline 10000 -I ..
CAML_DEP = ocamldep
CAML_LEX = ocamllex
CAML_YAC = ocamlyacc -v

LIBS =

CUSTOM = -custom

WITH_BYT_AGRAPHICS = ../agraphics.cma
WITH_BYT_GRAPHICS_PS = ../agraphics.cma
WITH_BYT_UNIX = unix.cma

all: depend byt bin

byt: $(BYT_EXECS)

bin: $(BIN_EXECS)

SOURCES1 = $(SOURCES:.mly=.ml)
SOURCES2 = $(SOURCES1:.mll=.ml)
BYT_OBJS = $(SOURCES2:.ml=.cmo)
BIN_OBJS = $(BYT_OBJS:.cmo=.cmx)

$(BYT_EXECS): $(BYT_OBJS)
	for i in $(EPS_PRG); do \
	  $(CAML_BYT) $(CUSTOM) -o $$i.byt $(WITH_BYT_GRAPHICS_PS) $(WITH_BYT_UNIX) $$i.cmo; \
	  ./$$i.byt > $$i.eps; \
	  echo "To visualize the result: ghostview, gs, or gv $$i.eps"; \
	done
	for i in $(PS_PRG); do \
	  $(CAML_BYT) $(CUSTOM) -o $$i.byt $(WITH_BYT_GRAPHICS_PS) $(WITH_BYT_UNIX) $$i.cmo; \
	  ./$$i.byt; \
	  echo "To visualize the result: ghostview, gs, or gv $$i.ps"; \
	done
	for i in $(EXECS); do \
	  $(CAML_BYT) $(CUSTOM) -o $$i $(WITH_BYT_AGRAPHICS) $(WITH_BYT_UNIX) $$i.cmo; \
	  ./$$i; \
	  echo "To visualize the result: apgv $$i.apg"; \
	done

$(BIN_EXECS): $(BIN_OBJS)
	for i in $(EPS_PRG); do \
	  $(CAML_BIN) -o $$i.bin $(WITH_BYT_GRAPHICS_PS:.cma=.cmxa) $$i.cmx; \
	  ./$$i.bin > $$i.eps; \
	  echo "To visualize the result: ghostview, gs, or gv $$i.eps"; \
	done
	for i in $(PS_PRG); do \
	  $(CAML_BIN) -o $$i.bin $(WITH_BYT_GRAPHICS_PS:.cma=.cmxa) $$i.cmx; \
	  ./$$i.bin; \
	  echo "To visualize the result: ghostview, gs, or gv $$i.ps"; \
	done
	for i in $(EXECS); do \
	  $(CAML_BIN) -o $$i.opt $(WITH_BYT_AGRAPHICS:.cma=.cmxa) $$i.cmx; \
	  ./$$i.opt; \
	  echo "To visualize the result: apgv $$i.apg"; \
	done

$(BYT_EXECS): ../agraphics.cma
$(BIN_EXECS): ../agraphics.cmxa

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
	rm -f *.cm[aiox]* *~ .*~ #*#
	rm -f *.o
	rm -f *.apg
	rm -f *.eps *.ps
	rm -f a.out $(BYT_EXECS)
	rm -f $(BIN_EXECS)

.depend: $(SOURCES2)
	$(CAML_DEP) *.mli *.ml > .depend

depend: $(SOURCES2)
	$(CAML_DEP) *.mli *.ml > .depend

include .depend
