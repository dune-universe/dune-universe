# $Id: Makefile.facile,v 1.2 2003/08/06 14:01:31 brisset Exp $

# Generic Makefile for single files using FaCiLe. Allow to produce
# file.out (ocamlc) and file.opt (ocamlopt) from file.ml while linking
# with the FaCiLe library

FACILEDIR= +facile
INCLUDES= -I $(FACILEDIR)

OCAMLC= ocamlc -g $(INCLUDES)
OCAMLMLI= ocamlc $(INCLUDES)
OCAMLOPT= ocamlopt $(INCLUDES)
OCAMLDEP= ocamldep $(INCLUDES)

jobshop.out : scheduling.cmo jobshop.cmo
	$(OCAMLC) -o $@ str.cma facile.cma $^
jobshop.opt : scheduling.cmx jobshop.cmx 
	$(OCAMLOPT) -o $@ str.cmxa facile.cmxa $^

.SUFFIXES:
.SUFFIXES: .ml .mli .mly .mll .cmi .cmo .cmx .out .opt .p.cmx .popt

.ml.cmo :
	$(OCAMLC) -c $<
.mli.cmi :
	$(OCAMLMLI) -c $<
.ml.cmx :
	$(OCAMLOPT) -c $<
# To produce profiled objects
.ml.p.cmx :
	$(OCAMLOPT) -p -c $<
	mv $*.cmx $@
	mv $*.o $*.p.o
.cmo.out :
	$(OCAMLC) -o $@ facile.cma $<
# To produce profiled binaries
.p.cmx.popt :
	$(OCAMLOPT) -p -o $@ facile.p.cmxa $<
.cmx.opt :
	$(OCAMLOPT) -o $@ facile.cmxa $<
.mly.ml :
	ocamlyacc $<
.mll.ml :
	ocamllex $<

clean:
	\rm -f *.cmo *.cmi *.cmx *.o *~ *.opt *.out .depend *.popt

.depend:
	$(OCAMLDEP) *.mli *.ml > $@

include .depend
