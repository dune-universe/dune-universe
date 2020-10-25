FADBADMLDIR ?= $(shell ocamlfind query fadbadml)
INCLUDES += -I $(FADBADMLDIR) fadbad.cmxa

cmx = \
	fode.cmx \
	interval.cmx \
	affineForm.cmx \
	sets.cmx \
	taylorModel.cmx \
	continuousIntegrator.cmx \
	exampleReachability.cmx

build: objects exampleReachability.opt

run: exampleReachability.opt exampleReachability.gp
	./exampleReachability.opt -tend 2.0 -dt 2e-2 -order 3 -threshold 3e-3 > data.dat
	gnuplot exampleReachability.gp
	xdg-open picture.svg

%.opt: %.ml
	ocamlopt -o $@ $(OCAMLOPTFLAGS) $(INCLUDES) $<

objects: $(cmx)

sets.cmx: interval.cmx affineForm.cmx

exampleReachability.opt: INCLUDES += interval.cmx affineForm.cmx sets.cmx taylorModel.cmx continuousIntegrator.cmx

clean_this:
	rm -rf *.cm[ixo] *.opt *.o .depend
	rm -f data.dat picture.svg

clean: clean_this
	$(MAKE) -C test clean

cleanall: clean_this
	$(MAKE) -C test cleanall

-include .depend
.depend:
	ocamldep *.ml *.mli > .depend
