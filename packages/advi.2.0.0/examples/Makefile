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

# The Makefile for the various example subdirectories.

# $Id$


EXPORTFILES = $(shell find -name CVS -prune -o -name '[^.]*' -print)


SUBDIRS=basics prosper seminar slitex


all:
	for i in $(SUBDIRS); do \
	 echo $$i; \
	 cd $$i; $(MAKE) all; cd ..; \
	done
	@echo '*** All examples have been successfully compiled'
	@echo '*** Run "make show" to view the examples'

show:
	for i in $(SUBDIRS); do \
	 echo $$i; \
	 cd $$i; $(MAKE) show; cd ..; \
	done

distdir:
	rm -rf distexamples
	cvs export -D today -d distexamples advi/examples
	find distexamples -name .cvsignore  -exec rm '{}' ';'
	rmdir $(distdir)
	mv distexamples/examples $(distdir) 
	rmdir distexamples

test:
	for i in $(SUBDIRS); do \
	 echo $$i; \
	 cd $$i; $(MAKE) all; cd ..; \
	done

clean:
	for i in $(SUBDIRS); do \
	 cd $$i; $(MAKE) clean; cd ..; \
	done
