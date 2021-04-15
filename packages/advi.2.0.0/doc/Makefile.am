#*********************************************************************#
#                                                                     #
#                             Active-DVI                              #
#                                                                     #
#                   Projet Cristal, INRIA Rocquencourt                #
#                                                                     #
#  Copyright 2002 Institut National de Recherche en Informatique et   #
#  en Automatique.  All rights reserved.  This file is distributed    #
#  under the terms of the GNU Lesser General Public License.          #
#                                                                     #
#  Jun Furuse, Didier Rémy and Pierre Weis.                           #
#  Contributions by Roberto Di Cosmo, Didier Le Botlan,               #
#  Xavier Leroy, and Alan Schmitt.                                    #
#                                                                     #
#  Based on Mldvi by Alexandre Miquel.                                #
#*********************************************************************#

# $Id$
# needed for autoconf < 2.60
dvidir = $(datadir)/doc/$(PACKAGE)
pdfdir = $(datadir)/doc/$(PACKAGE)
psdir = $(datadir)/doc/$(PACKAGE)
htmldir = $(datadir)/doc/$(PACKAGE)

man_MANS = advi.1
dvi_DATA = manual.dvi \
	splash.dvi \
	scratch_write_splash.dvi \
	scratch_draw_splash.dvi 
pdf_DATA = manual.pdf
ps_DATA  = manual.ps
html_DATA = $(HTMLFILES) $(STATICIMAGES) $(BUILTIMAGES)

TEXSOURCES = advi_remote.tex \
	     argv.tex \
	     manual.tex \
	     scratch_draw_splash.tex \
	     scratch_write_splash.tex \
	     scratch_draw_splash_contents.tex \
	     scratch_write_splash_contents.tex \
	     splash.tex \
	     splash_header.tex \
	     splash_contents.tex

HTMSOURCES = \
	index.htm screen-shots.htm faq.htm 

HTMLFILES = manual.html manualxxx.html \
	    $(HTMSOURCES:.htm=.html)


index.html: index.htm Makefile.am

STATICIMAGES = bar.gif \
	       inria.gif \
	       advilogo.gif \
	       advilogo.anim.gif

# Don't know what this was used for
# BUILTIMAGES = manual001.png \
# 	      manual002.png

# distributing produced files remove dependencies
# on Hevea, HTMLC and dvipdfm
EXTRA_DIST = $(man_MANS) $(dvi_DATA) $(pdf_DATA) $(ps_DATA) \
	     $(HTMLFILES) $(STATICIMAGES) $(BUILTIMAGES) \
	     manual.sty \
	     manual.hva \
	     macros.hva \
	     $(TEXSOURCES) \
	     $(HTMSOURCES) \
	     includes

dist-hook:
	rm -rf $(distdir)/includes/CVS

MAINTAINERCLEANFILES = $(man_MANS) $(dvi_DATA) $(pdf_DATA) $(ps_DATA) \
		       $(HTMLFILES) $(BUILTIMAGES) \
		       $(TEXSOURCES:.tex=.log) \
		       $(TEXSOURCES:.tex=.aux) \
		       $(TEXSOURCES:.tex=.out) \
		       manual.idx \
		       manual.toc \
		       manual.haux \
		       manual.htoc \
		       manual.hind \
		       manual.image.tex

SUBDIRS = pngs

# various flags
TEX_ENV		= TEXINPUTS=$(srcdir):$(top_srcdir)/tex:$(TEXINPUTS) \
		  TEXPSHEADERS=$(srcdir):$(top_srcdir)/tex:$(TEXPSHEADERS)
HEVEA_FLAGS	= -entities -I $(srcdir) -I $(top_srcdir)/tex
HTMLC_FLAGS	= -I $(srcdir)/includes \
		  -D USER '$(USER)' \
		  -D HOSTNAME 'yquem.inria.fr' \
		  -D CamlVersion '$(OCAMLVERSION)' \
		  -D version '$(VERSION)'

MV		= mv -f

manual.dvi: ../tex/advi.sty manual.sty splash_contents.tex \
       scratch_write_splash_contents.tex scratch_draw_splash_contents.tex
splash.dvi: splash.tex splash_header.tex splash_contents.tex
scratch_write_splash.dvi: scratch_write_splash_contents.tex
scratch_draw_splash.dvi: scratch_draw_splash_contents.tex

manual.html: manual.tex manual.sty manual.hva macros.hva ../tex/advi.hva \
       scratch_write_splash_contents.tex scratch_draw_splash_contents.tex
advi_remote.html: advi_remote.tex manual.sty macros.hva 

SUFFIXES = .tex .dvi .advi .ps .pdf .pdfm .html .htm .ima .man .1

# To avoid default hypertex.cfg if it exists.
# Active-DVI really needs the hypertex driver...
.tex.dvi:
	$(TEX_ENV) $(LATEX) '\def\driver{hypertex}\input' $<
	if [ -f $*.idx ]; then $(MAKEINDEX) $*; fi
	$(TEX_ENV) $(LATEX) '\def\driver{hypertex}\input' $<
	if [ -f $*.idx ]; then $(MAKEINDEX) $*; fi
	$(TEX_ENV) $(LATEX) '\def\driver{hypertex}\input' $<

.tex.ps:
if HAVE_DVIPS
	$(TEX_ENV) $(LATEX) '\def\driver{dvips}\input' $<
	if [ -f $*.idx ]; then $(MAKEINDEX) $*; fi
	$(TEX_ENV) $(LATEX) '\def\driver{dvips}\input' $<
	$(TEX_ENV) $(DVIPS) -o $@ $*.dvi
else
	@echo "dvips required, unable to proceed"
	@exit 1
endif

.tex.pdf:
if HAVE_DVIPDFM
	$(TEX_ENV) $(LATEX) '\def\driver{dvipdfm}\input' $<
	if [ -f $*.idx ]; then $(MAKEINDEX) $*; fi
	$(TEX_ENV) $(LATEX) '\def\driver{dvipdfm}\input' $<
	$(TEX_ENV) $(DVIPDFM) -o $@ $*.dvi
else
	@echo "dvipdfm required, unable to proceed"
	@exit 1
endif

.html.ima:
	if [ -f $*.image.tex ]; then $(TEX_ENV) $(PDFIMAGEN) $*; fi
	touch $@

.tex.html:
if HAVE_HEVEA
	 $(TEX_ENV) $(HEVEA) $(HEVEA_FLAGS) -fix $(srcdir)/macros.hva -o $@ $<
else
	@echo "hevea required, unable to proceed"
	@exit 1
endif

manualxxx.html: manual.html
if HAVE_HACHA
	$(TEX_ENV) $(HACHA) -o $@ $<
	sed -e 's,"manualxxx.html">several HTML files<,"manual.html">a single HTML file<,' $@ > $@.tmp
	mv $@.tmp $@
else
	@echo "hacha required, unable to proceed"
	@exit 1
endif

bindings.html: bindings.tex splash_contents.tex

bindings.txt: bindings.tex splash_contents.tex
if HAVE_HEVEA
	 $(TEX_ENV) $(HEVEA) $(HEVEA_FLAGS) -text \
		-fix $(srcdir)/macros.hva -o $@ $<
else
	@echo "hevea required, unable to proceed"
	@exit 1
endif


$(HTMLFILES): ../configure

.htm.html: 
	sed \
	 -e 's,@VERSION\@,$(VERSION),' \
	 -e 's,@COPYRIGHT\@,$(BUILD_YEAR),' \
	 $< > $@ 

.man.1: $(top_srcdir)/config.status
	sed \
	 -e 's,@VERSION\@,$(VERSION),' \
	 -e 's,@COPYRIGHT\@,$(BUILD_YEAR),' \
	 $< > $@ 
