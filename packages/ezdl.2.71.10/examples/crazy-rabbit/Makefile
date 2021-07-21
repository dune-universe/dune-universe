export OCAMLOPT=ocamlopt

MAIN=rabbit

LURETTE=lurette --test-length 500

EXPDIR=`lutin --ocaml-version`
$(EXPDIR):
	[ -d $(EXPDIR) ] || (mkdir -p $(EXPDIR) ; make utest)


%.cmxs: %.ml
	 ocamlfind ocamlopt -shared -o $*.cmxs  -package rdbg,graphics graphics.cmxa $*.ml 


clean:
	rm -rf *.ec *.cm* *.log *~ .*~ *.o *rif0 *rif Data *.pp_luc *.plot *.gp $(MAIN).opt


test_new.rif:$(EXPDIR) rabbit.cmxs
	rm -f test_new.rif0 .lurette_rc
	$(LURETTE) --output test_new.rif0  \
	  -sut "ocaml rabbit.cmxs" \
	  -env "lutin rabbit.lut -main rabbit -seed 34 -L libm.so.6 -loc" && \
	grep -v "lurette chronogram" test_new.rif0  | \
	grep -v "Version"   | \
	grep -v "#seed"   | \
	grep -v "The execution lasted"| sed -e "s/^M//" > test_new.rif 


test: test_new.rif $(EXPDIR)
	diff -B -u -i  $(EXPDIR)/test_new.rif.exp test_new.rif > test_new.res || true
	cat test_new.res
	[ ! -s test_new.res ]  && make clean

utest:test_new.rif
	cp test_new.rif $(EXPDIR)/test_new.rif.exp

# A FAIRE
#
# 1 - determiner les sommets de l'obstacle avec get_vertices pour pouvoir afficher le vrai obstacle
# 2 - Colorer l'obstacle en bleu pour pouvoir remettre les anciens points de
# la trajectoire
# 3 - Faire se ballader l'obstacle un peu plus aleatoirement
# 
