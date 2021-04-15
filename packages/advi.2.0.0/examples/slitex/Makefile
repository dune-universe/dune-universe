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

# The Makefile for the slitex sub-directory examples.

# $Id$

# To view the demos, use ../../advi.opt file.dvi

SUBDIRS=advi caml simplistic skin_detection

all:
	for i in $(SUBDIRS); do \
	  if test -d $$i; then \
	    cd $$i; $(MAKE) all; cd ..; \
	  fi; \
	done

show:
	for i in $(SUBDIRS); do \
	  if test -d $$i; then \
	    cd $$i; $(MAKE) show; cd ..; \
	  fi; \
	done

clean:
	$(RM) a.out *.aux *.log *.dvi *~ 
	for i in $(SUBDIRS); do \
	  if test -d $$i; then \
	    cd $$i; $(MAKE) clean; cd ..; \
	  fi; \
	done
