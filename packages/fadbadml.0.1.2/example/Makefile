FADBADMLDIR ?= $(shell ocamlfind query fadbadml)
INCLUDES += -I $(FADBADMLDIR) fadbad.cmxa

EXAMPLES = \
	exampleFAD.opt \
	exampleBAD.opt \
	exampleFADFAD.opt \
	exampleBADFAD1.opt \
	exampleBADFAD2.opt \
	exampleBADFAD3.opt \
	exampleBADFAD4.opt \
	exampleNewton.opt \
	exampleTAD1.opt \
	exampleTAD2.opt \
	exampleTAD3.opt \
	monteCarlo.opt \
	exampleString.opt \
	exampleBrusselator.opt

all: $(EXAMPLES)

%.opt: %.ml
	ocamlopt -o $@ $(OCAMLOPTFLAGS) $(INCLUDES) $<

exampleBrusselator.opt: INCLUDES += graphics.cmxa unix.cmxa
brusselator.tmp: exampleBrusselator.opt
	./exampleBrusselator.opt -notad -n 20000 -o brusselator.tmp
brusselator.png: brusselator.tmp
	gnuplot -e "set key autotitle columnhead; set terminal png size 800,800; set output '$@'; plot '$<' using 2:3"



clean:
	rm -rf *.cm[ixo] *.opt *.o .depend
	rm -rf *.tmp *.png
	$(MAKE) -C reachability clean

cleanall: clean

-include .depend
.depend:
	ocamldep *.ml *.mli > .depend
