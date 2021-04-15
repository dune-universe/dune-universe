#*********************************************************************#
#                                                                     #
#                             Active-DVI                              #
#                                                                     #
#                   Projet Cristal, INRIA Rocquencourt                #
#                                                                     #
#  Copyright 2004 Institut National de Recherche en Informatique et   #
#  en Automatique.  All rights reserved.  This file is distributed    #
#  under the terms of the GNU Lesser General Public License.          #
#                                                                     #
#  Jun Furuse, Didier Rémy and Pierre Weis.                           #
#  Contributions by Roberto Di Cosmo, Didier Le Botlan,               #
#  Xavier Leroy, and Alan Schmitt.                                    #
#                                                                     #
#  Based on Mldvi by Alexandre Miquel.                                #
#*********************************************************************#

# $Id Exp $

# include ../Makefile.config
LATEX=latex

# binaries
ADVI	=../advi

# flags
TEX_ENV	= TEXINPUTS=.:../tex:$$TEXINPUTS
ADVI_ENV= TEXPSHEADERS=../tex:

# sources
TEXFILES= Zeitoun.tex
DVIFILES= $(TEXFILES:%.tex=%.dvi)

all: $(DVIFILES)

$(DVIFILES): bar.eps ../tex/advi.sty ../tex/advi-annot.sty

.SUFFIXES: .tex .sty .dvi .ps .eps .advi

.dvi.advi:
	$(ADVI_ENV) $(ADVI) $<

.tex.dvi:
	$(TEX_ENV) $(LATEX) $<

bar.eps: ../tex/bar.eps
	cp -p ../tex/bar.eps .

clean::
	$(RM) a.out *.toc *.aux *.log *.dvi *~ 
	$(RM) .advi/*
