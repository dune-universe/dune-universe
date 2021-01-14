PICO=picosat-960
QUANT=quantor-3.2

all: $(PICO)/libpicosat.a $(QUANT)/libquantor.a


$(QUANT)/config.h: $(PICO)/libpicosat.a
	cd $(dir $@); ./configure --picosat=../$(PICO)
$(PICO)/config.h:
	cd $(dir $@); ./configure

# Even though we only build a static library, we need the flag '-fPIC'.
# The -fPIC flag is needed because ocaml-qbf will be built as a shared lib
# (as well as static lib). The shared lib need position-independent code.
$(PICO)/libpicosat.a: $(PICO)/config.h
	cd $(dir $@);\
	make CFLAGS="-fPIC $$(grep CFLAGS= makefile | cut -d= -f2)" libpicosat.a;\
	ranlib libpicosat.a
$(QUANT)/libquantor.a: $(QUANT)/config.h
	cd $(dir $@);\
	make CFLAGS="-fPIC $$(grep CFLAGS= Makefile | cut -d= -f2)" libquantor.a

clean:
	make -C $(PICO) clean
	make -C $(QUANT) clean

.PHONY: all clean
