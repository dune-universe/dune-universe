# Time-stamp: <modified the 29/06/2021 (at 14:13) by Erwan Jahier> 

ifndef OCAMLOPT
export OCAMLOPT=ocamlfind ocamlopt
endif

RDBG=rdbg-top 
RDBG_BATCH=rdbg-batch

%.cmxs: %.ml
	$(OCAMLOPT) -shared -o $*.cmxs  -package lustre-v6 -linkpkg  $*.ml 


clean:
	rm -rf *.ec *.cm* *.c *.log *~ .*~ *.o *rif0 *rif Data *.plot *.gp  *.res *.dot *.ps *.pdf *.exec *.out *.svg *.dro *.h *.c *.sh *session*ml


# There is 4 possible way to use the same test.ml file
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

rdbg:
	rdbg -sut "lv6 heater_control.lus -n heater_control" \
        -env "lv6 heater_env.lus -n heater_env"


test1.rif:$(EXPDIR) test.cmxs
	$(RDBG_BATCH) test.cmxs -o test1.rif0 && \
	grep -v "Version" test1.rif0 | \
	grep -v "The random engine was initialized with" | \
	sed -e "s/^M//" > test1.rif

test1: test1.rif $(EXPDIR)/test1.rif.exp
	rm -f test1.res
	diff -B -u -i  $(EXPDIR)/test1.rif.exp test1.rif > test1.res
	cat test1.res
	[ ! -s test1.res ]


utest1:test1.rif
	cp test1.rif $(EXPDIR)/test1.rif.exp

test2.rif:$(EXPDIR) test.cmxs
	$(RDBG_BATCH) -lurette test.cmxs -l 50 -o test2.rif0
	grep -v "Version" test2.rif0  | \
	grep -v "The random engine was initialized with"   | \
	sed -e "s/^M//" > test2.rif 

test2: test2.rif $(EXPDIR)/test2.rif.exp
	rm -f test2.res
	diff -B -u -i  $(EXPDIR)/test2.rif.exp test2.rif > test2.res || true
	cat test2.res
	[ ! -s test2.res ]

# the same as test2, but using the v6 compiler instead of the interpreter!
# i need to patch the generated loop file so that it boots as usual env (ie it
# start with using inputs.
utest2:test2.rif
	cp test2.rif $(EXPDIR)/test2.rif.exp

test2b.rif:$(EXPDIR) 
	lv6 heater_control.lus -n heater_control -2c -cc;
	lv6 heater_env.lus -n heater_env -2c;
	cat heater_env_heater_env_loop.c | sed s/"Heat_on = _get_bool"/"if (_s>1)  Heat_on = _get_bool"/ > loop.c
	mv loop.c heater_env_heater_env_loop.c
	sh heater_env.sh && \
	$(RDBG_BATCH) -lurette \
		--sut-stdio "./heater_control.exec " \
		--env-stdio "./heater_env.exec " \
		-l 50 -o test2b.rif0 && \
	grep -v "Version" test2b.rif0  | \
	grep -v "The random engine was initialized with"   | \
	sed -e "s/^M//" > test2b.rif 

test2b: test2b.rif $(EXPDIR)/test2b.rif.exp
	rm -f test2b.res
	diff -B -u -i  $(EXPDIR)/test2b.rif.exp test2b.rif > test2b.res || true
	cat test2b.res
	[ ! -s test2b.res ]

utest2b:test2b.rif
	cp test2b.rif $(EXPDIR)/test2b.rif.exp


test3.rif:  $(EXPDIR) 
	rm -f test3.rif0
	$(RDBG) -init test_lurette_mode.ml
	grep -v "Version" test3.rif0  | \
	grep -v "The random engine was initialized with"   | \
	sed -e "s/^M//" > test3.rif 

test3: test3.rif $(EXPDIR)/test3.rif.exp
	rm -f test3.res
	diff -B -u -i  $(EXPDIR)/test3.rif.exp test3.rif > test3.res || true
	cat test3.res
	[ ! -s test3.res ]

utest3:test3.rif
	cp test3.rif $(EXPDIR)/test3.rif.exp

test4.rif:  $(EXPDIR) 
	rm -f test4.rif0
	$(RDBG) -init test_rdbg_mode.ml   
	grep -v "Version" test4.rif0  | \
	grep -v "The random engine was initialized with"   | \
	sed -e "s/^M//" > test4.rif 

test4: test4.rif $(EXPDIR)/test4.rif.exp
	rm -f test4.res
	diff -B -u -i  $(EXPDIR)/test4.rif.exp test4.rif > test4.res || true
	cat test4.res
	[ ! -s test4.res ]

utest4:test4.rif
	cp test4.rif $(EXPDIR)/test4.rif.exp

# test1 test2 are broken since Dynlink checks for already imported modules (4.10.0 ?)
test: test3 test4 clean
utest: utest3 utest4

############################################################################



time1: testTime.cmxs
	time $(RDBG_BATCH) testTime.cmxs -o testTime1.rif0 > out1

time2: testTime.cmxs
	time $(RDBG_BATCH) -lurette testTime.cmxs -o testTime2.rif0 > out2

time3:  
	rm -f testTime3.rif0
	time $(RDBG) -init testTime_lurette_mode.ml > out3

time4:  
	rm -f testTime4.rif0
	time $(RDBG) -init testTime_rdbg_mode.ml > out4

time: time1 time2 time3 time4
