CONFIG = ../_build/default/src/Makefile.config

TEXINPUTS = .:../tex/:

DEPS = $(shell ls *.tex ../tex/*.hva ../tex/*.eps *.sty *.hva) 

OUTDIR = _build

LATEX = TEXINPUTS=$(TEXINPUTS) latexmk -outdir=$(OUTDIR)

MANFILE = advi.1
SPLASHFILES = splash.dvi scratch_draw_splash.dvi scratch_write_splash.dvi

DISTFILES = $(MANFILE) $(SPLASHFILES)
DISTBUILD = $(patsubst %, $(OUTDIR)/%, $(DISTFILES))

MANUALFILES = advi.pdf advi_remote.pdf advi.html advixxx.html 
MANUALBUILD = $(patsubst %, $(OUTDIR)/%, $(MANUALFILES))


.PHONY: all manual distbuild distclean promote \
	install install.manual uninstall clean

all: $(DISTFILES)

manual: $(MANUALBUILD)

$(OUTDIR)/.exists:
	mkdir -p $(OUTDIR)
	touch $(@)


$(CONFIG): 
	make -C .. src/Makefile.config

include $(CONFIG)

$(OUTDIR)/%.dvi: %.tex $(DEPS)
	$(LATEX) -dvi $<

$(OUTDIR)/%.pdf: %.tex $(DEPS)
	$(LATEX) -ps $<
	ps2pdf $(OUTDIR)/advi.ps $@

$(OUTDIR)/advi.html: advi.tex manual.hva $(DEPS) $(OUTDIR)/.exists
	hevea -entities -I . -I ../tex -I $(OUTDIR) -o $@ -fix manual.hva $<

$(OUTDIR)/advixxx.html: $(OUTDIR)/advi.html 
	cd $(OUTDIR) && hacha -o advixxx.html advi.html

$(OUTDIR)/%.md: %.in.md $(OUTDIR)/.exists
	LC_CTYPE=C sed -e 's,@@VERSION@@,$(package_version),' $< > $@ 

$(OUTDIR)/advi.1: $(OUTDIR)/advi.md  $(OUTDIR)/.exists
	pandoc --standalone --from markdown $< --to man --output $@

distbuild: $(DISTBUILD)

distclean:
	rm -f $(DISTFILES)

promote: $(DISTBUILD)
	for i in $(DISTFILES); do ln -f $(OUTDIR)/$$i .; done

clean:  
	rm -rf _build


install: $(MANFILE) $(SPLASHFILES)
	install -d $(advi_mandir)/man1/ $(advi_docdir)/
	install $(MANFILE) $(advi_mandir)/man1/
	install $(SPLASHFILES) $(advi_docdir)/

MANUALEXTRA = advi.css contents_motif.svg next_motif.svg previous_motif.svg

install.manual: $(MANUALBUILD)
	install -d $(advi_docdir)/
	for i in $(MANUALFILES) $(MANUALEXTRA); \
	  do install $(OUTDIR)/$$i $(advi_docdir)/; done
	install $(OUTDIR)/advi0??.html $(advi_docdir)/
	install $(OUTDIR)/advixxx.html $(advi_docdir)/index.html

uninstall: 
	rm -vf $(advi_mandir)/man1/advi.1
	rm -vf $(advi_docdir)/index.html 
	rm -vf $(advi_docdir)/advi???.html 
	for i in $(SPLASHFILES) $(MANUALFILES) $(MANUALEXTRA); \
	  do rm -vf $(advi_docdir)/$$i; done

RSYNC = rsync --chmod=u+w,o-w,o+rX 

include ../web/Makefile.website

website: $(MANUALBUILD)
	$(RSYNC) $(MANUALBUILD) $(patsubst %, $(OUTDIR)/%, $(MANIALEXTRA)) $(WEBSITEDIR)
