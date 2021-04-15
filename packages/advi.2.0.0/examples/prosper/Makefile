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

# The Makefile for the prosper examples.

# $Id$

# To view the demos, use ../advi.opt file.dvi

SUBDIRS=Join LL

all:
	for i in $(SUBDIRS); do \
	  cd $$i; $(MAKE) all; cd ..; \
	done

show:
	for i in $(SUBDIRS); do \
	  cd $$i; $(MAKE) show; cd ..; \
	done

clean:
	$(RM) a.out *.aux *.log *.dvi *~ 
	for i in $(SUBDIRS); do \
	  cd $$i; $(MAKE) clean; cd ..; \
	done
