ifndef OCAMLOPT
export OCAMLOPT=ocamlfind ocamlopt
endif

LIBDIR=-I +rdbg
RDBG=rdbg-top  $(LIBDIR) 
RDBG_BATCH=rdbg-batch
LUSTREV6=lv6

EXPDIR=`$(RDBG) -version`
EXPDIR=$(shell $(OCAMLOPT) -version | cut -d " " -f5)


$(EXPDIR):
	[ -d $(EXPDIR) ] || (mkdir -p $(EXPDIR) ; make utest)

clean:
	rm -rf *.ec *.cm* *.log *~ .*~ *.o *rif0 *rif Data *.pp_luc *.plot *.gp *.c *.h *.dro


test1.rif:$(EXPDIR) 
	$(RDBG_BATCH) --luciole -lurette -l 2 -o test1.rif0  \
		--sut-stdio "$(LUSTREV6) -rif -exec  heater_control.lus -n heater_control" && \
	grep -v "Rdbg Version" test1.rif0  | \
	sed -e "s/^M//" > test1.rif 

test1: test1.rif $(EXPDIR)/test1.rif.exp
	rm -f test1.res
	diff -B -u -i  $(EXPDIR)/test1.rif.exp test1.rif > test1.res || true
	cat test1.res
	[ ! -s test1.res ] && make clean


utest1:test1.rif
	cp test1.rif $(EXPDIR)/test1.rif.exp


test: test1 
utest: utest1


