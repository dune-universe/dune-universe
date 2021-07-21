
IDLFILES = vector matrix poly
CAMLIDL=camlidl

idl : vector.ml matrix.ml poly.ml
# camlidl
%_caml.c %.ml %.mli: %.idl sedscript
	mkdir -p tmp
	cp $*.idl tmp
	$(CAMLIDL) -nocpp -I . tmp/$*.idl
	cp tmp/$*_stubs.c $*_caml.c
	sed -f sedscript tmp/$*.ml >$*.ml
	sed -f sedscript tmp/$*.mli >$*.mli

clean:
	/bin/rm -f $(IDLFILES:%=%_caml.c) $(IDLFILES:%=%.ml) $(IDLFILES:%=%.mli) polka_parser.ml polka_parser.mli polka_lexer.ml sedscript


sedscript: 
	echo "\
s/vector__t/t/g; \
s/matrix__t/t/g; \
s/poly__t/t/g; \
s/dimsup_t/dimsup/g; \
s/equation_t/equation/g; \
s/external vector_/external /g; \
s/external matrix_/external /g; \
s/external poly_/external /g; \
s/Tbool_bottom/Bottom/; \
s/Tbool_true/True/; \
s/Tbool_false/False/; \
s/Tbool_top/Top/; \
	" >$@

poly_caml.c poly.ml poly.mli: matrix.idl vector.idl
matrix_caml.c matrix.ml matrix.mli: vector.idl

