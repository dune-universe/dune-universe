CONFIG = ../_build/default/src/Makefile.config

advi_latexdir = $(advi_texdir)/tex/latex

SOURCES = \
  advi-annot.sty \
  advi-graphicx.sty \
  advi-slides.sty \
  advi.hva \
  advi.sty \
  argv.sty \
  bubble.sty \
  pgfsys-dvips.def \
  superpose.sty \
  xwindows-colors.sty \
  caml.eps \
  bar.eps \
  advilogo.eps

OLDSOURCES =  advi.pro

all: 

$(CONFIG):
	(cd ../src && dune build Makefile.config)

include $(CONFIG)

clean:  

install: $(SOURCES) 
	install -d $(advi_latexdir)
	install  $(SOURCES) $(advi_latexdir)

uninstall: 
	for i in $(SOURCES); do rm -vf $(advi_latexdir)/$$i; done
	if test -d $(advi_latexdir); then rmdir $(advi_latexdir); fi

