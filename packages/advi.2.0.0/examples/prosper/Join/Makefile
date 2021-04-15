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

# The Makefile for the Join example.

# $Id$

# To view the demos, use ../../../advi.opt file.dvi

TOPDIR=../../..
FILES=join.tex 
include ../../lib/Makefile

all:: join.dvi

show: join.dvi
	$(ADVI) join.dvi

join.dvi: join.tex
	$(LATEX) $<

join.pdf: join.tex
	$(LATEX) '\def\argv{ps2pdf}\input' $<
	dvips -o $*.ps $*.dvi && ps2pdf $*.ps $*.pdf
	rm $*.dvi

clean::
	/bin/rm -f join.aux join.log join.dvi join.ps join.pdf *~

.phony: show

