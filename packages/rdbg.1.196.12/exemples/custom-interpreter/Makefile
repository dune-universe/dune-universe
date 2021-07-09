
ifndef OCAMLOPT
export OCAMLOPT=ocamlfind ocamlopt
endif

LIBDIR=$(shell ocamlfind query lustre-v6 -i-format -r) 

RDBG_BATCH=rdbg-batch
CMXA=lutils.cmxa Lv6Run.cmxa


demo:lus2licLoc.cmxs
	echo "lv6 chrono_v4.lus -n chrono_v4 -exec" | rdbg-batch lus2licLoc.cmxs 

clean:
	rm -rf *.ec *.cm* *.log *~ .*~ *.o *rif0 *rif Data *.pp_luc *.plot *.gp *.c *.h *.dro

################################################################################
# Test

$(EXPDIR):
	[ -d $(EXPDIR) ] || (mkdir -p $(EXPDIR) ; make utest)


test.rif:$(EXPDIR) lus2licLoc.cmxs
	echo "lv6 chrono_v4.lus -n chrono_v4" | rdbg-batch lus2licLoc.cmxs -l 2 -o test.rif && \
	grep -v "Rdbg Version" test.rif0  | \
	sed -e "s/^M//" > test.rif 

test1: test.rif $(EXPDIR)/test.rif.exp
	rm -f test.res
	diff -B -u -i  $(EXPDIR)/test.rif.exp test.rif > test.res || true
	cat test.res
	[ ! -s test.res ]

test:
	echo "test unplugged for the time being"


utest: $(EXPDIR) test.rif
	cp test.rif $(EXPDIR)/test.rif.exp

%.cmxs: %.ml
	$(OCAMLOPT) -shared -o $*.cmxs  $(LIBDIR) $(CMXA) $*.ml 


