CONFIG = ../_build/default/src/Makefile.config

HTM = $(shell ls *.htm)

DEPS = $(TEX) $(STY) $(EPS)

OUTDIR = _build
 
WEBFILES = $(patsubst %, $(OUTDIR)/%l, $(HTM))

include Makefile.website


.PHONY: default website clean

default: $(WEBFILES) pngs/.small

pngs/.small:
	make -C pngs 


$(OUTDIR)/.exists:
	mkdir -p $(OUTDIR)
	touch $(@)

$(CONFIG): 
	make -C .. src/Makefile.config

include $(CONFIG)

$(OUTDIR)/%.html: %.htm $(OUTDIR)/.exists
	LC_CTYPE=C sed -e 's,@@VERSION@@,$(package_version),' $< > $@ 


clean:  
	rm -rf _build
	make -C pngs clean

RSYNC = rsync --chmod=u+w,o-w,o+rX 

website: default
	$(RSYNC)  $(ROOTFILES) \
	          _build/*.html \
                  style.css \
	          *.gif \
	          *.jpg \
            $(WEBSITEDIR)
	$(RSYNC) -a pngs $(WEBSITEDIR)
