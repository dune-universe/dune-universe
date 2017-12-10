# $Id: Makefile,v 1.73 2004/09/01 12:33:49 barnier Exp $

OCAMLC = ocamlc -g
OCAMLMLI = ocamlc
OCAMLOPT = ocamlopt -unsafe -noassert -inline 10
OCAMLDEP = ocamldep
INCLUDES =
OPTOPT = 
OCAMLLIB := $(shell echo `ocamlc -where`)
FACILELIB = $(OCAMLLIB)/facile

CSTR= fcl_debug.ml fcl_misc.ml fcl_domain.ml fcl_setDomain.ml fcl_float.ml fcl_stak.ml fcl_data.ml fcl_cstr.ml fcl_var.ml fcl_invariant.ml fcl_reify.ml fcl_boolean.ml fcl_linear.ml fcl_nonlinear.ml fcl_expr.ml fcl_arith.ml fcl_conjunto.ml fcl_goals.ml fcl_fdArray.ml fcl_gcc.ml fcl_opti.ml fcl_alldiff.ml fcl_sorting.ml fcl_interval.ml fcl_genesis.ml facile.ml
# fcl_binary.ml fcl_ac6.ml

CSTRCMO = $(CSTR:.ml=.cmo)
CSTRCMX = $(CSTR:.ml=.cmx)

# Default target: libraries and toplevel
all : facile.cma facile.cmxa facile

# Libraries
facile.cma : $(CSTRCMO)
	$(OCAMLC) -o $@ -a $(CSTRCMO)

facile.cmxa : $(CSTRCMX)
	$(OCAMLOPT) -o $@ -a $(CSTRCMX)

# Version of the library to be linked with the -p (profiler) option
facile.p.cmxa : $(CSTR)
	rm -fr $(CSTRCMX)
	make facile.cmxa OPTOPT=-p
	rm -fr $(CSTRCMX)
	mv facile.cmxa $@
	mv facile.a facile.p.a

# Toplevel (to be run with the "-I +facile" option)
facile : facile.cma
	ocamlmktop -o $@ facile.cma

# Experimental: to get inlining of functors using preprocessing:
# Does not work with many modules (e.g. containing class expressions
facile.exp.cmxa : $(CSTR)
	rm -fr $(CSTRCMX)
	make fcl_stak.cmx fcl_cstr.cmx fcl_inv.cmx fcl_reify.cmx fcl_arith.cmx fcl_goals.cmx fcl_fdArray.cmx fcl_gcc.cmx fcl_alldiff.cmx fcl_sorting.cmx fcl_interval.cmx facile.cmx
	make facile.cmxa OPTOPT="-pp inline_functors"
	rm -fr $(CSTRCMX)
	mv facile.cmxa $@
	mv facile.a facile.exp.a

.SUFFIXES:
.SUFFIXES: .ml .mli .mly .mll .cmi .cmo .cmx .p.cmx .s

.ml.cmo :
	$(OCAMLC) $(INCLUDES) $(INCLUDES) -c $<
.mli.cmi :
	$(OCAMLMLI) $(INCLUDES) -c $<
.ml.cmx :
	$(OCAMLOPT) $(OPTOPT) $(INCLUDES) -c $<
.ml.s :
	$(OCAMLOPT) $(OPTOPT) $(INCLUDES) -S -c $<
.mly.ml :
	ocamlyacc $<
.mll.ml :
	ocamllex $<

clean:
	\rm -f *.cm* *.annot *.o *.a *~ .depend facile

.depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > $@

include .depend
