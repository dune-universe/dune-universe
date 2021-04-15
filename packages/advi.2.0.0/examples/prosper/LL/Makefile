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

# The Makefile for the LL example.

# $Id$

# To view the demos, use ../../../advi.opt file.dvi

TEXINPUTS := ..:../../lib:../../../tex::
ADVI=../../../src/advi
LATEX=TEXINPUTS=$(TEXINPUTS) latex 

all: ll.dvi

show: ll.dvi
	$(ADVI) ll.dvi

ll.dvi: ll.tex world.jpg advilogo.anim.gif
	$(LATEX) $<

ll.pdf: ll.tex
	$(LATEX) '\def\argv{ps2pdf}\input' $<
	dvips -t landscape -o ll.ps ll.dvi && ps2pdf ll.ps ll.pdf
	./pdfrotate.pl ll.pdf
	rm ll.dvi

clean:
	/bin/rm -f ll.out ll.aux ll.log ll.dvi ll.ps ll.pdf *~

.phony: show

