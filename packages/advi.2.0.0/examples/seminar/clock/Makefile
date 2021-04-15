#*********************************************************************#
#                                                                     #
#                             Active-DVI                              #
#                                                                     #
#                   Projet Cristal, INRIA Rocquencourt                #
#                                                                     #
#  Copyright 2003 Institut National de Recherche en Informatique et   #
#  en Automatique.  All rights reserved.  This file is distributed    #
#  under the terms of the GNU Lesser General Public License.          #
#                                                                     #
#  Jun Furuse, Didier Rémy and Pierre Weis.                           #
#  Contributions by Roberto Di Cosmo, Didier Le Botlan,               #
#  Xavier Leroy, and Alan Schmitt.                                    #
#                                                                     #
# Pierre Weis                                                         #
#                                                                     #
#*********************************************************************#

# The Makefile for the clock example.

# $Id$

# To view the demos, use ../../../advi.opt file.dvi

TOPDIR=../../..
FILES=tools.tex
EPS=bar.jpg.eps cameleon.eps cameleon2.eps hump.eps logo.gif.eps ocaml.eps ocamldoc.eps ODBC.eps

include ../../lib/Makefile

all:: tools.dvi

show:: tools.dvi
	advi tools.dvi

tools.dvi: $(EPS) tools.tex
	$(LATEX) tools.tex

tools.pdf: $(EPS) tools.tex
	$(LATEX) '\def\argv{ps2pdf}\input' tools.tex
	dvips -o $*.ps $*.dvi && ps2pdf $*.ps $*.pdf
	rm $*.dvi 

$(EPS):$(EPS.eps=.eps.gz)
	gunzip -c $*.eps.gz > $@

clean::
	rm -f *.dvi *.pdf *.aux *.log *.ps *~ $(EPS)

.phony: show
