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

# The Makefile for the A14 example.

# $Id$

# To view the demos, use ../advi.opt file.dvi

N=14
ADVIDIR=../../..
LATEX=TEXINPUTS=.:$(ADVIDIR)/tex:$(TEXINPUTS): latex

all:: a${N}.dvi

show:: a${N}.dvi
	advi a${N}.dvi

a${N}.dvi: mon-dvipdf.tex a${N}.tex
	./putimage <a${N}.tex | gpic -t > a${N}bis.tex
	- $(LATEX) a${N}bis.tex
	mv -f a${N}bis.dvi a${N}.dvi

mon-dvipdf.tex:
	echo "" > mon-dvipdf.tex
	${MAKE} clear

ps: a${N}.tex a${N}.dvi
	dvips -f a${N}.dvi >a${N}.ps

light:
	echo "\usepackage{colors-light}" > mon-color.tex

clear:
	echo "\usepackage{colors-clear}" > mon-color.tex

dark:
	echo "\usepackage{colors-dark}" > mon-color.tex

html: mon-dvipdf.tex
	./putimage <a${N}.tex |gpic -t > a${N}bis.tex
	hevea sem.hva a${N}bis.tex
	${MAKE} images
	mv -f a${N}bis.html a${N}.html

images:
	imagen -extra "pnmflip -ccw" -mag 1700 a${N}bis

pdf:
	echo "\usepackage[dvipdfm]{hyperref}" >mon-dvipdf.tex
	/bin/rm -f a${N}.dvi
	${MAKE} a${N}.dvi
	dvipdfm a${N}.dvi 

PUBLIC_HTML=ma-machine:/usr/home/moi/public_html

web: html
	scp a${N}.html a${N}.ps a${N}.pdf a${N}*.gif ${PUBLIC_HTML}/cours/a${N}

clean::
	/bin/rm -f *~ *.dvi *.ps *.pdf *.html \
        *.log *.aux *.out *.gif \
	a${N}bis.tex a${N}bis.image.tex mon-color.tex mon-dvipdf.tex

.phony: show
