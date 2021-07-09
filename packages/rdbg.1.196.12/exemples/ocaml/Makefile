ifndef OCAMLOPT
export OCAMLOPT=ocamlfind ocamlopt
endif
ifndef OCAMLC
export OCAMLC=ocamlfind ocamlc
endif


#RDBGTOP=rdbg-top $(LIBDIR)  
#RDBG_BATCH=rdbg-batch 

ifndef RDBG_TOP
RDBG_TOP=rdbg-top 
endif

# the local one does work because ocamlfind 
RDBG_BATCH=rdbg-batch


test.cmxs: env_in_ocaml.ml sut_in_ocaml.ml test.ml
	 $(OCAMLOPT) -g -shared -package rdbg -o test.cmxs  $^

%.cma: %.ml
	 $(OCAMLC) -custom -a -o $*.cma -package rdbg  $*.ml 

clean:
	rm -rf *.ec *.cm* *.log *~ .*~ *.o *rif0 *rif Data *.pp_luc *.plot *.gp 


# There is 4 possible ways to use the test.ml file
# |--------------+-------+------------|
# |              | batch | interactif |
# |--------------+-------+------------|
# | rdbg mode    | test1 | test4      |
# | lurette mode | test2 | test3      |
# |--------------+-------+------------|

EXPDIR=`$(RDBG) -version`
EXPDIR=$(shell $(OCAMLOPT) -version | cut -d " " -f5)

$(EXPDIR):
	[ -d $(EXPDIR) ] || (mkdir -p $(EXPDIR) ; make utest)

test1.rif:$(EXPDIR) test.cmxs
	$(RDBG_BATCH)   test.cmxs -o test1.rif0 && \
	grep -v "Rdbg Version" test1.rif0  | \
	sed -e "s/^M//" > test1.rif 

test1: test1.rif $(EXPDIR)/test1.rif.exp
	rm -f test1.res
	diff -B -u -i  $(EXPDIR)/test1.rif.exp test1.rif > test1.res || true
	cat test1.res
	[ ! -s test1.res ]


utest1:test1.rif
	cp test1.rif $(EXPDIR)/test1.rif.exp

test2.rif:$(EXPDIR) test.cmxs
	$(RDBG_BATCH) test.cmxs -lurette -o test2.rif0 && \
	grep -v "Rdbg Version" test2.rif0  | \
	sed -e "s/^M//" > test2.rif 

test2: test2.rif $(EXPDIR)/test2.rif.exp
	rm -f test2.res
	diff -B -u -i  $(EXPDIR)/test2.rif.exp test2.rif > test2.res || true
	cat test2.res
	[ ! -s test2.res ] && make clean

utest2:test2.rif
	cp test2.rif $(EXPDIR)/test2.rif.exp


test3.rif:  $(EXPDIR) sut_in_ocaml.cma env_in_ocaml.cma test_lurette_mode.ml
	rm -f test3.rif0
	$(RDBG_TOP) sut_in_ocaml.cma env_in_ocaml.cma test_lurette_mode.ml && \
	grep -v "lurette chronogram" test3.rif0  | \
	grep -v "Rdbg Version"   | \
	grep -v "Reading "   | \
	sed -e "s/^M//" > test3.rif 

test3: test3.rif $(EXPDIR)/test3.rif.exp
	rm -f test3.res
	diff -B -u -i  $(EXPDIR)/test3.rif.exp test3.rif > test3.res || true
	cat test3.res
	[ ! -s test3.res ]

utest3:test3.rif

	cp test3.rif $(EXPDIR)/test3.rif.exp

LIBDIR=-I +rdbg `ocamlfind query rdbg -r -i-format` 

test4.rif:  $(EXPDIR) sut_in_ocaml.cma env_in_ocaml.cma test.ml
	rm -f test4.rif0
	$(RDBG_TOP)  $(LIBDIR) sut_in_ocaml.cma env_in_ocaml.cma test.ml  > test4.rif0  
	grep -v "lurette chronogram" test4.rif0  | \
	grep -v "Rdbg Version"   | \
	grep -v "Reading "   | \
	sed -e "s/^M//" > test4.rif 

test4: test4.rif $(EXPDIR)/test4.rif.exp
	rm -f test4.res
	diff -B -u -i  $(EXPDIR)/test4.rif.exp test4.rif > test4.res || true
	cat test4.res
	[ ! -s test4.res ] && make clean

utest4:test4.rif
	cp test4.rif $(EXPDIR)/test4.rif.exp

test: test1 test2 test3 test4
utest: utest1 utest2 utest3 utest4


