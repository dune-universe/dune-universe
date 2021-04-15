#*********************************************************************#
#                                                                     #
#                             Active-DVI                              #
#*********************************************************************#
#                                                                     #
#                   Projet Cristal, INRIA Rocquencourt                #
#                                                                     #
#  Copyright 2002 Institut National de Recherche en Informatique et   #
#  en Automatique.  All rights reserved.  This file is distributed    #
#  under the terms of the GNU Lesser General Public License.          #
#                                                                     #
#  Jun Furuse, Didier Rémy and Pierre Weis.                           #
#  Contributions by Roberto Di Cosmo, Didier Le Botlan,               #
#  Xavier Leroy, and Alan Schmitt.                                    #
#                                                                     #
#  Based on Mldvi by Alexandre Miquel.                                #
#*********************************************************************#

# $Id$

# To view the demo, use ../advi demo.dvi
# include ../Makefile.config

# OUTDIR = ../_build/default/test/
OUTDIR = _build/

# binaries
ADVI = ../advi

# flags
TEXINPUTS = .:../tex:

LATEX = TEXINPUTS=$(TEXINPUTS) latexmk -output-directory=$(OUTDIR)
PLATEX = TEXINPUTS=$(TEXINPUTS) latexmk -latex=platex -output-directory=$(OUTDIR)


# sources
TEXFILES= demo.tex demo2.tex trigo_source.tex \
	  trans.tex trans2.tex trans3.tex background.tex \
	  color.tex demo_raw.tex macros.tex test_events.tex

DVIFILES= $(TEXFILES:%.tex=%.dvi)

# Those example do not compile anymore (not maintained)
# include Makefile.more

all :: $(DVIFILES) $(MLPROG)


$(DVIFILES): bar.eps demo.sty ../tex/advi.sty ../tex/advi-annot.sty

%.advi: %.dvi 
	$(ADVI) $<

%.dvi: %.tex bar.eps
	$(LATEX) $<
	ln -f $(OUTDIR)/$@ .

jpdemo.dvi: jpdemo.tex
	$(PLATEX) $<
	ln -f $(OUTDIR)/$@ .

bar.eps: ../tex/bar.eps
	cp -p ../tex/bar.eps .

clean::
	rm -rf _build
	rm -f $(DVIFILES)

install:

uninstall:

.SUFFIXES: .tex .sty .dvi .ps .eps .advi
