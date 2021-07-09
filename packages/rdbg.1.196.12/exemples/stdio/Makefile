ifndef OCAMLOPT
export OCAMLOPT=ocamlfind ocamlopt
endif

RDBG=rdbg-top
RDBG_BATCH=rdbg-batch


ifndef LV6
LV6=lv6
endif
ifndef LUTIN
LUTIN=lutin
endif


EXPDIR=`$(RDBG) -version`
EXPDIR=$(shell $(OCAMLOPT) -version)


$(EXPDIR):
	[ -d $(EXPDIR) ] || (mkdir -p $(EXPDIR) ; make utest)

clean:
	rm -rf *.ec *.cm* *.log *~ .*~ *.o *rif0 *rif Data *.plot *.gp *.res



######################### how to use rdbg-batch
test1.rif:$(EXPDIR) 
	$(RDBG_BATCH) -lurette -l 10 -o test1.rif0  \
		--env-stdio "$(LUTIN) -boot -rif -seed 42 degradable-sensors.lut -m main" \
		--sut-stdio "$(LV6) -exec -rif heater_control.lus -n heater_control" \
	&& grep -v "Rdbg Version" test1.rif0  | \
	sed -e "s/^M//" > test1.rif 

test1: test1.rif $(EXPDIR)/test1.rif.exp
	rm -f test1.res
	diff -B -u -i  $(EXPDIR)/test1.rif.exp test1.rif > test1.res || true
	cat test1.res
	[ ! -s test1.res ] && make clean


utest1:test1.rif
	cp test1.rif $(EXPDIR)/test1.rif.exp

# the same thing using the lustre compiler instead of the interpreter
test2.rif:$(EXPDIR)
	$(LV6) -2c -cc heater_control.lus -n heater_control; 
	$(RDBG_BATCH) -lurette -l 10 -o test2.rif0  \
		--env-stdio "$(LUTIN) -boot -rif -seed 42 degradable-sensors.lut -m main" \
		--sut-stdio "./heater_control.exec" && \
	grep -v "Rdbg Version" test2.rif0  | \
	sed -e "s/^M//" > test2.rif 

test2: test2.rif $(EXPDIR)/test2.rif.exp
	rm -f test2.res
	diff -B -u -i  $(EXPDIR)/test2.rif.exp test2.rif > test2.res || true
	cat test2.res
	[ ! -s test2.res ] && make clean


utest2: $(EXPDIR) test2.rif
	cp test2.rif $(EXPDIR)/test2.rif.exp

utest3: $(EXPDIR) test3.rif
	cp test3.rif $(EXPDIR)/test3.rif.exp


######################### how to use lurette
test3.rif:
	lurette -env "./stdio-script.sh" -sut "./stdio-script-alt.sh" -o test3.rif0
	grep -v "Rdbg Version" test3.rif0  | sed -e "s/^M//" > test3.rif

test3: test3.rif $(EXPDIR)


test: test1 test2 test3
utest: utest1 utest2 utest3


