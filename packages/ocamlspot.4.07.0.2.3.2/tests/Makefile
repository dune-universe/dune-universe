# include ../../config/Makefile

# Various commands and dir
##########################
OCAMLC   = ocamlc -annot -bin-annot
OCAMLOPT = ocamlopt -annot -bin-annot
OCAMLDEP = ocamldep -slash
OCAMLLEX = ocamllex
OCAMLYACC= ocamlyacc

# Compilation
#############
OCAMLSRCDIR=..
INCLUDES_DEP=-I dir1 -I dir2

# Requires unix!
COMPFLAGS= $(INCLUDES_DEP)

include Makefile.targets

all: test2.cmo $(TARGETS) 

pack.cmo: packed.cmo
	$(OCAMLC) -pack -o $@ $<

packed.cmx: packed.ml
	$(OCAMLOPT) -for-pack Pack $<   	

packed.cmo: packed.ml
	$(OCAMLC) -for-pack Pack $<   	

pack.cmx: packed.cmx
	$(OCAMLOPT) -pack -o $@ $<

packed_alias.cmo: pack.cmo

packed_alias.cmx: pack.cmx

test2.cmo: test.cmo
	$(OCAMLC) -pack -o $@ $<

dir1/test4.cmo: dir1/test4.ml
	$(OCAMLC) -c -I dir1 -o $@ $<

test5.cmo: test5.ml
	$(OCAMLC) -c -I dir1 -o $@ $<

dir2/test6.cmo: dir2/test6.ml
	$(OCAMLC) -c -I dir2 -o $@ $<

camlp4_path.cmo: camlp4_path.ml
	$(OCAMLC) -c -pp camlp4o -o $@ $<

clean:
	rm -f *.cm* *.o *.annot *.sp*t  */*.cm* */*.annot */*.sp*t

# generic rules :
#################

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLPP) $(COMPFLAGS) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.mli:
	$(OCAMLYACC) -v $<

beforedepend::

depend: beforedepend
	$(OCAMLDEP) $(INCLUDES) -I dir1 -I dir2 *.mli *.ml */*.mli */*.ml > .depend

Makefile.targets: *.ml *.mll *.mli */*.ml
	echo TARGETS= \\ > $@
	ls *.ml *.mli *.mll | sed -e 's/mli$$/cmi/' -e 's/ml$$/cmo/' -e 's/mll$$/cmo/' -e 's/$$/ \\/'	| grep -v partial >> $@

.PHONY: clean install installopt beforedepend depend

ocamlspot:
	ln -s ../ocamlspot ocamlspot

include .depend
