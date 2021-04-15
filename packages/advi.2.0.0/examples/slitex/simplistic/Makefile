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

# The Makefile for the slitex simplistic sub-directory example.

# $Id$

# To view the demos, use ../../../advi.opt file.dvi

LATEX=latex

OSTYLES=../../../tex

.SUFFIXES: .dvi .tex

all: talk.dvi

show: talk.dvi
	advi talk.dvi

clean:
	/bin/rm -f *.z[io] *~ *.txt *.log *.aux *.dvi talk.ps spelling

scratch: clean
	/bin/rm -f *.dvi; rm -f *.bbl

talk.dvi: talk.tex

.tex.dvi:
	@ TEXINPUTS=$(OSTYLES):$$TEXINPUTS; export TEXINPUTS; latex $<

ps:
	dvips -o talk.ps -t landscape talk.dvi
