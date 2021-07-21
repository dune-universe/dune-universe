
LTOP=lurette_old

LURETTETOP=$(LTOP)    \
	   --test-length 100 --thick-draw 1 \
	  --draw-inside 0 --draw-edges 0 --draw-vertices 0 --draw-all-vertices  \
	  --step-mode Inside --local-var --no-gnuplot --no-sim2chro   \
	  --do-not-show-step 

EXPDIR=`$(LTOP) --ocaml-version`
$(EXPDIR):
	[ -d $(EXPDIR) ] || (mkdir -p $(EXPDIR) ; make utest)



# several oracles
test1.rif: heater_control$(EXE) $(EXPDIR) 
	rm -f test1.rif0 .lurette_rc
	$(LURETTETOP) -go --output test1.rif0 --seed 42 \
	  -rp "sut:ec_exe:./heater_control.ec:"  \
	  -rp "oracle:v6:heater_control.lus:not_a_sauna"  \
	  -rp "oracle:v6:heater_control.lus:not_a_fridge"  \
	  -rp "env:lutin:env.lut:-m:main:-seed:3" && \
	grep -v "lurette chronogram" test1.rif0  | \
	grep -v "This is lurette Version" test1.rif0  | grep -v "#seed" | \
	grep -v "The execution lasted"| sed -e "s/^M//" > test1.rif 

test1: test1.rif $(EXPDIR)
	rm -f test1.res
	diff  -u -i  $(EXPDIR)/test1.rif.exp test1.rif > test1.res || true
ifneq (,$(findstring $(HOSTTYPE),win32))
	taskkill /F /IM ecexe.exe || true
	taskkill /F /IM heater_control.exe || true
endif
	cat test1.res
	[ ! -s test1.res ] 


test2.rif:
	rm -f test2.rif0 .lurette_rc
	$(LURETTETOP) -go  --output test2.rif0  \
	  -rp "sut:v6:heater_control.lus:heater_control:"  \
	  -rp "env:lutin:degradable-sensors.lut:-m:main:-seed:3" && \
	grep -v "lurette chronogram" test2.rif0  | \
	grep -v "This is lurette Version" test2.rif0  | grep -v "#seed" |\
	grep -v "The execution lasted"| sed -e "s/^M//" > test2.rif

test2:test2.rif $(EXPDIR)
	rm -f test2.res	
	diff  -u -i  $(EXPDIR)/test2.rif.exp test2.rif > test2.res || true
ifneq (,$(findstring $(HOSTTYPE),win32))
	taskkill /F /IM ecexe.exe || true
endif
	cat test2.res
	[ ! -s test2.res ] 


# broken (4/11/2013)
# Calling the C step function (coming from lus2c) via a cmxs
test3.rif: heater_control.cmxs
	rm -f test3.rif0 .lurette_rc
	$(LURETTETOP) --luciole -go  --output test3.rif0  \
	  -rp "sut:ocaml:heater_control.cmxs:"  \
	  -rp "env:lutin:degradable-sensors.lut:-m:main:-seed:3" && \
	grep -v "lurette chronogram" test3.rif0  | \
	grep -v "This is lurette Version" test3.rif0  |grep -v "#seed" | \
	grep -v "The execution lasted"| sed -e "s/^M//" > test3.rif 

test3:test3.rif  $(EXPDIR)
	rm -f test3.res
	diff  -u -i  $(EXPDIR)/test3.rif.exp test3.rif > test3.res || true
	cat test3.res
	[ ! -s test3.res ] && make clean

# several env
test4.rif: heater_control$(EXE)
	rm -f test4.rif0 .lurette_rc
	$(LURETTETOP) -go --output test4.rif0  --seed 3 \
	  -rp "sut:ec_exe:./heater_control.ec:"  \
	  -rp "env:lutin:env.lut:-m:main1"  \
	  -rp "env:lutin:env.lut:-m:main2" && \
	grep -v "lurette chronogram" test4.rif0  | \
	grep -v "This is lurette Version" test4.rif0  | grep -v "#seed" | \
	grep -v "The execution lasted"| sed -e "s/^M//" > test4.rif 

test4:test4.rif $(EXPDIR)
	rm -f test4.res 
	diff  -u -i  $(EXPDIR)/test4.rif.exp test4.rif > test4.res || true
ifneq (,$(findstring $(HOSTTYPE),win32))
	taskkill /F /IM heater_control.exe || true
endif
	cat test4.res
	[ ! -s test4.res ] 



utest1:test1.rif
	cp test1.rif $(EXPDIR)/test1.rif.exp

utest2:test2.rif
	cp test2.rif $(EXPDIR)/test2.rif.exp

utest4:test4.rif
	cp test4.rif $(EXPDIR)/test4.rif.exp


utest: utest1 utest2 utest4 

test: test1 test2  test4 

%.ec: %.lus
	../../bin/lus2lic$(EXE) -ec $^ -n $* -o $@

.PHONY:heater_control$(EXE)
heater_control$(EXE): heater_control.ec 
	../../bin/ec2c$(EXE) -loop heater_control.ec 
	gcc heater_control.c heater_control_loop.c -o $@ 

%.c: %.ec
	../../bin/ec2c$(EXE) -loop $^

LIB=-I ../../lib graphics.cmxa ocaml2c.cmx ocamlRM.cmx -I +rdbg-plugin rdbg-plugin.cmxa
XXX=-I ../../source/obj-linux/

# Essai pour appeler via des cmxs du code C généré par lus2c
# Bon, ca semble ne pas marcher (au 4/11/2013)
# XXX 
ocaml2c_stubs.c:
	camlidl ocaml2c.idl

# XXX modify gen_stubs to generate different names
lurette__sut.c:
	gen_stubs heater_control.lus heater_control verimag .


rm.c:heater_control$(EXE) lurette__sut.c 
	cat lurette__sut.c | sed -e 's/\"float/\"real/' > rm.c
# lustreRM.ml should be elsewhere 
%.cmxs: rm.c %.c lustreRM.ml %_ml.ml ocaml2c_stubs.c
	 ocamlopt -shared -o $*.cmxs $(XXX)  $(LIB)  $^


sut:
	call-via-socket -addr 127.0.0.1 -port 2000 -server ./not_a_sauna.sh




clean:
	rm -rf *.h *.c *.ec *.log *~ .*~ *.o *rif0 *rif Data *.pp_luc *.plot *.gp heater_control not_a_sauna* *.cov *.cm* *.dro



test_dontgo:
	rm -f test.rif0 .lurette_rc
	$(LURETTETOP)  --output test.rif0 env.lut && \
	grep -v "lurette chronogram" test.rif0  | \
	grep -v "The execution lasted"| sed -e "s/^M//" | grep -v "#seed"  > test.rif  &&\
	rm -f test.res && diff  -u -i  $(EXPDIR)/test.rif.exp test.rif > test.res
	[ ! -s test.res ] && make clean 




