#*********************************************************************#
#                                                                     #
#                             Active-DVI                              #
#                                                                     #
#                   Projet Cristal, INRIA Rocquencourt                #
#                                                                     #
#  Copyright 2002-2004,                                               #
#  Institut National de Recherche en Informatique et en Automatique.  #
#  All rights reserved. This file is distributed under the terms of   #
#  the GNU Lesser General Public License.                             #
#                                                                     #
#  Jun Furuse, Didier Rémy and Pierre Weis.                           #
#  Contributions by Roberto Di Cosmo, Didier Le Botlan,               #
#  Xavier Leroy, and Alan Schmitt.                                    #
#                                                                     #
#  Based on Mldvi by Alexandre Miquel.                                #
#*********************************************************************#

# $Id$

stubdir = @OCAMLLIB@/stublibs
stub_DATA = 

bin_PROGRAMS =
if BUILD_NATIVE
bin_PROGRAMS += advi
endif
if BUILD_BYTECODE
stub_DATA += dlladvi.so
bin_PROGRAMS += advi.byt
endif


OCAMLBUILTSOURCES = config.ml
OCAMLSOURCES = misc.ml timeout.ml ageometry.ml \
	       options.ml rc.ml userfile.ml \
	       graphicsY11.ml global_options.ml busy.ml gradient.ml gterm.ml \
	       launch.ml dvicolor.ml shot.ml laser_pointer.ml \
	       symbol.ml \
	       input.ml table.ml pkfont.ml ttfont.ml jfm.ml search.ml \
	       font.ml glyph.ml devfont.ml adviUnits.ml dimension.ml dvi.ml \
	       drawimage.ml gs.ml transimpl.ml embed.ml grdev.ml addons.ml \
	       scratch.ml cdvi.ml driver.ml thumbnails.ml dviview.ml main.ml
OCAMLINTERFACES = misc.mli timeout.mli ageometry.mli \
		  options.mli rc.mli userfile.mli \
		  graphicsY11.mli global_options.mli busy.mli gradient.mli \
		  gterm.mli \
		  launch.mli dvicolor.mli shot.mli laser_pointer.mli \
		  symbol.mli \
		  input.mli table.mli pkfont.mli ttfont.mli jfm.mli \
		  search.mli \
		  font.mli glyph.mli devfont.mli adviUnits.mli dimension.mli \
		  dvi.mli dvicommands.mli \
		  drawimage.mli gs.mli transitions.mli embed.mli grdev.mli \
		  addons.mli \
		  scratch.mli cdvi.mli driver.mli thumbnails.mli dviview.mli

CSOURCES = events.c grwm.c grY11.c
CHEADERS = grwm.h image.h libgraph.h
advi_SOURCES = $(OCAMLSOURCES) $(OCAMLINTERFACES) $(CSOURCES) $(CHEADERS)
advi_byt_SOURCES = $(OCAMLSOURCES) $(OCAMLINTERFACES) $(CSOURCES) $(CHEADERS)

BUILT_SOURCES = $(OCAMLBUILTSOURCES)

EXTRA_DIST = config.ml.in

CLEANFILES = $(OCAMLBYTOBJS) \
	     $(OCAMLOPTOBJS) \
	     $(OCAMLOPTOBJS:.cmx=.o) \
	     $(OCAMLBUILTSOURCES:.ml=.cmi) \
	     $(OCAMLSOURCES:.ml=.cmi) \
	     dvicommands.cmi \
	     transitions.cmi \
	     $(COBJS) \
	     config.ml \
	     .depend

OCAMLBYTOBJS    = $(OCAMLBUILTSOURCES:.ml=.cmo) $(OCAMLSOURCES:.ml=.cmo)
OCAMLOPTOBJS    = $(OCAMLBUILTSOURCES:.ml=.cmx) $(OCAMLSOURCES:.ml=.cmx)
COBJS           = $(CSOURCES:.c=.o)

AM_CFLAGS	= $(XINERAMA_CFLAGS) $(X_CFLAGS)
AM_OCAMLCFLAGS	= $(CAMLIMAGES_INCLUDES)

OTHER_LIBS	= unix str graphics

OCAMLLIBS_OPT	= $(OTHER_LIBS:=.cmxa) $(CAMLIMAGES_LIBS_NATIVE) 
OCAMLLIBS_BYT	= $(OTHER_LIBS:=.cma) $(CAMLIMAGES_LIBS_BYTE) 

# In native mode no need to dynamically link with dlladvi
advi$(EXEEXT): $(COBJS) $(OCAMLOPTOBJS)
	$(OCAMLOPT) -o advi$(EXEEXT) \
		$(OCAMLCFLAGS) $(AM_OCAMLCFLAGS) $(AM_LINKFLAGS) \
		$(COBJS) $(OCAMLLIBS_OPT) $(OCAMLOPTOBJS) \
		$(XINERAMA_CCLIB) $(CAMLIMAGES_CCLIBS)

# In bytecode mode create stubcode in dlladvi
DLLADVI = dlladvi.so
dlladvi.so: $(COBJS) Makefile
	$(OCAMLMKLIB) -o advi \
		$(OCAMLCFLAGS) $(AM_OCAMLCFLAGS) $(AM_LINKFLAGS) \
		$(COBJS) \
		$(CAMLIMAGES_MKLIBS) $(CAMLIMAGES_LPATH) $(XINERAMA_MKLIB) 

advi.byt$(EXEEXT): dlladvi.so $(OCAMLBYTOBJS)
	$(OCAMLC) -o advi.byt$(EXEEXT) \
		$(OCAMLCFLAGS) $(AM_OCAMLCFLAGS) $(AM_LINKFLAGS) \
		$(OCAMLLIBS_BYT) $(OCAMLBYTOBJS) \
		-dllib -ladvi

install_hook: $(INSTALLEXEC) installdata
	- texhash
	@ if test -z "`kpsewhich advi.sty`"; then \
	  echo '*** NOTE BEFORE USE ***' ;\
	  echo Please add $(ADVI_LOC); \
	  echo to your TEXINPUTS environment variable\!; \
	  echo '***********************' ;\
	fi

config.ml: config.ml.in $(top_srcdir)/config.status
	rm -f $@ $@.tmp
	sed \
		-e 's,@PACKAGE_VERSION\@,$(PACKAGE_VERSION),g' \
		-e 's,@BUILD_DATE\@,$(BUILD_DATE),g' \
		-e 's,@HAVE_GS\@,$(HAVE_GS),g' \
		-e 's,@HAVE_CAMLIMAGES\@,$(HAVE_CAMLIMAGES),g' \
		-e 's,@TEXMFMAIN\@,$(TEXMFMAIN),g' \
		-e 's,@dvidir\@,$(dvidir),g' \
		-e 's,@etcdvidir\@,$(sysconfdir)/advi,g' \
		-e 's,@GUNZIP\@,$(GUNZIP),g' \
		-e 's,@BUNZIP\@,$(BUNZIP),g' \
		-e 's,@GS\@,$(GS),g' \
		$< > $@.tmp
	mv $@.tmp $@

SUFFIXES = .ml .mli .cmo .cmi .cmx .c .o

.ml.cmo:
	$(OCAMLC) $(OCAMLCFLAGS) $(AM_OCAMLCFLAGS) -o $@ -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLCFLAGS) $(AM_OCAMLCFLAGS) -o $@ -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLCFLAGS) $(AM_OCAMLCFLAGS) -o $@ -c $<

.c.o: $(top_srcdir)/config.h
	$(OCAMLC) -ccopt "-o $@ $(CFLAGS) $(AM_CFLAGS)" -c $< 

# moving to $(srcdir) is needed in case of VPATH build
.depend: $(OCAMLSOURCES) $(OCAMLINTERFACES) $(top_srcdir)/config.status
	(cd $(srcdir) && $(OCAMLDEP) $(OCAMLSOURCES) $(OCAMLINTERFACES)) > .depend 

# ugly stuff for make time include
@AMDEP_TRUE@@am__include@ @am__quote@.depend@am__quote@
