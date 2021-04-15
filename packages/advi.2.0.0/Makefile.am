SUBDIRS = src tex doc
# SUBDIRS = src 
DIST_SUBDIRS = $(SUBDIRS) examples

CVSRELEASETAG = Release-$(subst .,_,$(VERSION))
ANNOUNCEFILE  = Announce-$(VERSION)

WEBSITEDIR = /net/yquem/infosystems/www/cristal/cristal/$(PACKAGE)
FTPSITEDIR = /net/yquem/infosystems/ftp/$(PACKAGE)
CVSROOTDIR = /net/yquem/devel/caml/repository/
CVSPKGDIR  = bazar-ocaml/$(PACKAGE)

EXTRA_DIST = ocaml.m4 conf/jpfonts.conf LGPL

cvstag:
	cvs tag -c $(CVSRELEASETAG)

cvsuntag:
	cvs tag -d $(CVSRELEASETAG)

announce:
	mail -n -s "[ANN] New release $(VERSION) of $(NAME)" \
		advi@inria.fr \
		caml-announce@inria.fr \
	 < $(ANNOUNCEFILE)

.PHONY: test test.keep

test: test.keep
	rm -r test

test.keep: dist
	rm -rf test
	mkdir test
	cd test && tar -xf ../$(DIST_ARCHIVES) && \
	cd $(distdir) && configure &&  make

FILEPERMS = -g cristal -m 644
DIRERMS = -g cristal -m 755
ROOTFILES = README INSTALL LICENSE ChangeLog
website: webdist webonly

webdist: dist 
	if [ -d $(WEBSITEDIR) ]; then     \
	: ; \
	else                              \
		$(INSTALL) -d $(DIRPERMS) $(WEBSITEDIR); \
		$(INSTALL) -d $(DIRPERMS) $(WEBSITEDIR)/archives/; \
	fi
	$(INSTALL) $(FILEPERMS) $(DIST_ARCHIVES) $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) $(DIST_ARCHIVES) $(WEBSITEDIR)/archive/

webonly: dist
	$(INSTALL) $(FILEPERMS) $(ROOTFILES) $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/*.html $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/manual*.png $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/*.css $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/*.dvi $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/*.ps $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/*.pdf $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/*.gif $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/*.jpg $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/advi.1 $(WEBSITEDIR)

