# Time-stamp: <modified the 30/06/2021 (at 16:26) by Erwan Jahier> 

ifndef OCAMLOPT
export OCAMLOPT=ocamlfind ocamlopt
endif

ifndef RDBG_TOP
	RDBG_TOP=rdbg-top
endif
ifndef RDBG_BATCH
	RDBG_BATCH=rdbg-batch
endif

%.cmxs: %.ml
	$(OCAMLOPT) -shared -o $*.cmxs  -package rdbg,lutin -linkpkg  $*.ml 


clean:
	rm -rf *.ec *.cm* *.log *~ .*~ *.o *rif0 *rif Data  *.res 
	rm -rf rdbg-session* *.gp luretteSession* rdbg-cmds.ml *.seed

# There is 4 possible ways to use the same test.ml file
# |--------------+-------+------------|
# |              | batch | interactif |
# |--------------+-------+------------|
# | lurette mode | test2 | test3      |
# | rdbg mode    | test1 | test4      |
# |--------------+-------+------------|

EXPDIR=`$(RDBG_TOP) -version`
EXPDIR=$(shell $(OCAMLOPT) -version)

$(EXPDIR):
	[ -d $(EXPDIR) ] || (mkdir -p $(EXPDIR) ; make utest)

test1.rif:$(EXPDIR)  test.cmxs
	$(RDBG_BATCH) test.cmxs -o test1.rif0
	grep -v "Version" test1.rif0  | \
	sed -e "s/^M//" > test1.rif 

test1: test1.rif $(EXPDIR)/test1.rif.exp
	rm -f test1.res
	diff -B -u 0 -i  $(EXPDIR)/test1.rif.exp test1.rif > test1.res || true
	cat test1.res
	[ ! -s test1.res ]


utest1:test1.rif
	cp test1.rif $(EXPDIR)/test1.rif.exp

test2.rif:$(EXPDIR) test.cmxs
	$(RDBG_BATCH) -lurette test.cmxs -l 50   -o test2.rif0
	grep -v "Version" test2.rif0  | \
	sed -e "s/^M//" > test2.rif 

test2: test2.rif $(EXPDIR)/test2.rif.exp
	rm -f test2.res
	diff -B -u 0 -i  $(EXPDIR)/test2.rif.exp test2.rif > test2.res || true
	cat test2.res
	[ ! -s test2.res ]

utest2:test2.rif
	cp test2.rif $(EXPDIR)/test2.rif.exp


test3.rif:  $(EXPDIR)  test_lurette_mode.ml test.ml
	rm -f test3.rif0
	$(RDBG_TOP) -init  test_lurette_mode.ml
	grep -v "Version" test3.rif0  | \
	sed -e "s/^M//" > test3.rif 

test3: test3.rif $(EXPDIR)/test3.rif.exp
	rm -f test3.res
	diff -B -u 0 -i  $(EXPDIR)/test3.rif.exp test3.rif > test3.res || true
	cat test3.res
	[ ! -s test3.res ]

utest3:test3.rif
	cp test3.rif $(EXPDIR)/test3.rif.exp

test4.rif:  $(EXPDIR)  test.ml test_rdbg_mode.ml
	rm -f test4.rif0
	$(RDBG_TOP) -init test_rdbg_mode.ml 
	grep -v "Version" test4.rif0  | \
	sed -e "s/^M//" > test4.rif 

test4: test4.rif $(EXPDIR)/test4.rif.exp
	rm -f test4.res
	diff -B -u 0 -i  $(EXPDIR)/test4.rif.exp test4.rif > test4.res || true
	cat test4.res
	[ ! -s test4.res ]

utest4:test4.rif
	cp test4.rif $(EXPDIR)/test4.rif.exp

.PHONY:test1 test2 test3 test4
# test1 test2 are broken since Dynlink checks for already imported modules (4.10.0 ?)
test:  test3 test4 clean
#utest: utest1 utest2 utest3 utest4
utest: utest3 utest4

rdbg:
	rdbg --emacs -sut "lutin crazy_controleur.lut -main heater_control -seed 42" -env "lutin degradable-sensors.lut -main main -seed 42"

############################################################################

time1: testTime.cmxs
	time $(RDBG_BATCH) testTime.cmxs -o testTime1.rif0 > out1

time2: testTime.cmxs
	time $(RDBG_BATCH) -lurette testTime.cmxs -l 50   -o testTime2.rif0 > out2

time3:  
	rm -f testTime3.rif0
	time $(RDBG_TOP) -init testTime_lurette_mode.ml > out3

time4:  
	rm -f testTime4.rif0
	time $(RDBG_TOP) -init testTime_rdbg_mode.ml > out4

time: time1 time2 time3 time4
