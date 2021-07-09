ifndef OCAMLOPT
export OCAMLOPT=ocamlfind ocamlopt
endif

RDBG=rdbg-top
RDBG_BATCH=rdbg-batch


ifndef LUS2LIC
LUS2LIC=lv6
endif
ifndef LUTIN
LUTIN=lutin
endif


EXPDIR=`$(RDBG) -version`
EXPDIR=$(shell $(OCAMLOPT) -version)


$(EXPDIR):
	[ -d $(EXPDIR) ] || (mkdir -p $(EXPDIR) ; make utest)

clean:
	rm -rf *.ec *.cm* *.log *~ .*~ *.o *rif0 *rif Data *.plot *.gp *.res *.c *.h *.sh *.exec



test1.rif:$(EXPDIR) 
	freeport1="`python -c 'import socket; s=socket.socket(); s.bind(("", 0)); print(s.getsockname()[1]); s.close()')`";\
	freeport2="`python -c 'import socket; s=socket.socket(); s.bind(("", 0)); print(s.getsockname()[1]); s.close()')`";\
	call-via-socket -server -port $$freeport1 "$(LUTIN) -boot -rif -seed 42 degradable-sensors.lut -m main" & \
	call-via-socket -server -port $$freeport2 "$(LUS2LIC) -exec -rif heater_control.lus -n heater_control" & \
	$(RDBG_BATCH)  -lurette -l 10 -o test1.rif0  \
		--env-socket "127.0.0.1:$$freeport1" \
		--sut-socket "127.0.0.1:$$freeport2" &&\
	grep -v "Rdbg Version" test1.rif0  | \
	grep -v "# env"   | \
	grep -v "# sut"   | \
	sed -e "s/^M//" > test1.rif 

test1: test1.rif $(EXPDIR)/test1.rif.exp
	rm -f test1.res
	diff -B -u -i  $(EXPDIR)/test1.rif.exp test1.rif > test1.res || true
	cat test1.res
	[ ! -s test1.res ] && make clean


utest1:test1.rif
	cp test1.rif $(EXPDIR)/test1.rif.exp

con:
	S="`ls`";\
	echo bllklezer="$$S"


# the same thing using the lustre compiler instead of the interpreter
test2.rif:$(EXPDIR)
	freeport1="`python -c 'import socket; s=socket.socket(); s.bind(("", 0)); print(s.getsockname()[1]); s.close()')`";\
	freeport2="`python -c 'import socket; s=socket.socket(); s.bind(("", 0)); print(s.getsockname()[1]); s.close()')`";\
	call-via-socket -server -port $$freeport1 "$(LUTIN) -boot -rif -seed 42 degradable-sensors.lut -m main" & \
	call-via-socket -server  -port $$freeport2 "$(LUS2LIC) -2c-exec heater_control.lus -n heater_control" & \
	$(RDBG_BATCH) -lurette -l 10 -o test2.rif0  \
		--env-socket "127.0.0.1:$$freeport1" \
		--sut-socket "127.0.0.1:$$freeport2" && \
	grep -v "Rdbg Version" test2.rif0  | \
	grep -v "# env"   | \
	grep -v "# sut"   | \
	sed -e "s/^M//" > test2.rif 

test2: test2.rif $(EXPDIR)/test2.rif.exp
	rm -f test2.res
	diff -B -u -i  $(EXPDIR)/test2.rif.exp test2.rif > test2.res || true
	cat test2.res
	[ ! -s test2.res ] && make clean


utest2:test2.rif
	cp test2.rif $(EXPDIR)/test2.rif.exp


test: test1 test2
utest: utest1 utest2


