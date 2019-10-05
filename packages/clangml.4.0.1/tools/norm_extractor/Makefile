DUNE_PREFIX ::= _build/default
NORM_PREFIX ::= ../norms/cplusplus/draft/source

norm_files ::= $(shell sed -n -e 's/^\\include{\([^}]*\)}/\1/p' $(NORM_PREFIX)/std.tex)

.PHONY : all
all : $(DUNE_PREFIX)/src/norm_extractor.exe extracted_norm.ml

.PHONY : $(DUNE_PREFIX)/src/norm_extractor.exe
$(DUNE_PREFIX)/src/norm_extractor.exe :
	dune build src/norm_extractor.exe

extracted_norm.ml : $(DUNE_PREFIX)/src/norm_extractor.exe
	$(DUNE_PREFIX)/src/norm_extractor.exe -o $@ \
		$(norm_files:%=$(NORM_PREFIX)/%.tex)
