FADBADMLDIR ?= $(shell ocamlfind query fadbadml)
INCLUDES += -I $(FADBADMLDIR) fadbad.cmxa

all: test_interval.opt test_affineForm.opt

%.opt: %.ml
	ocamlopt -o $@ $(OCAMLOPTFLAGS) $(INCLUDES) $<

test_interval.opt: ../interval.cmx
test_interval.opt: INCLUDES += -I ../ interval.cmx

test_affineForm.opt: ../affineForm.cmx
test_affineForm.opt: INCLUDES += -I ../ affineForm.cmx

clean:
	rm -rf *.cm[ixo] *.opt *.o .depend

cleanall: clean

-include .depend
.depend:
	ocamldep *.ml *.mli > .depend
