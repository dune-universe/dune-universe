TEXINPUTS := .:$(TOPDIR)/examples/lib:$(TOPDIR)/tex:$(TOPDIR)/examples/prosper:
ADVI=TEXPSHEADERS=.:$(TOPDIR)/tex: $(TOPDIR)/src/advi
LATEX=TEXINPUTS=$(TEXINPUTS) latex 
STY=advi.sty advi-annot.sty bubble.sty 
LIBSTY=commandline.sty xprosper.sty
DEPENDS=$(addprefix $(TOPDIR)/tex/, $(STY)) \
        $(addprefix $(TOPDIR)/examples/lib/, $(LIBSTY))
DVIFILES=$(FILES:%.tex=%.dvi) Makefile

all:: $(DVIFILES)

.dvi.advi:
	$(ADVI) $<

.tex.dvi: 
	$(LATEX) $*

.tex.ps:
	$(LATEX) '\def\argv{driver=dvips}\input' $*
	dvips -o $@ $*.dvi
	rm $*.dvi

.tex.pdf:
	$(LATEX) '\def\argv{driver=dvipdfm}\input' $*
	dvipdfm -o $@ $*.dvi
	rm $*.dvi

clean::
	rm -f $(FILES:%.tex=%.{aux,log,dvi,ps,pdf})

.SUFFIXES: .tex .dvi .advi .ps .eps .ftex .gz .pdf
