OCAMLMAKEFILE = ../OCamlMakefile

SOURCES = variant.ml 
LIB_PACK_NAME = variantslib
INCDIRS = +camlp4
PACKS = type-conv
RESULT = variantslib

TRASH = \
  pa_variants_conv.cmi pa_variants_conv.cmo pa_variants_conv.annot \

all: \
	pack-byte-code pack-native-code \
	variantslib.cma varianslib.cmxa \
	pa_variants_conv.cmi pa_variants_conv.cmo

#toplevel-library:	byte-code-library \
#	$(OCAMLC) -a -o sexplib_top.cma sexplib.cma install_printers.cmo

LIBINSTALL_FILES = \
  variantslib.cmi variantslib.cma variantslib.cmxa $(wildcard variantslib.a) \
  $(wildcard variantslib.lib) pa_variants_conv.cmi pa_variants_conv.cmo

install:	libinstall
uninstall:	libuninstall

clean::	clean-doc

-include $(OCAMLMAKEFILE)
