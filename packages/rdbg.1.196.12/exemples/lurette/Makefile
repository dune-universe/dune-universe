# test rdbg (the rdbg-top wrapper)
ifndef OCAMLOPT
export OCAMLOPT=ocamlfind ocamlopt
endif

RDBG=rdbg-top 
RDBG_BATCH=rdbg-batch

test:test1 clean
utest:utest1


####################################################################################
# The following works if you have installed tools (rdbg, lutin, etc) via git or opam
%.cmxs: %.ml
	$(OCAMLOPT) -shared -o $*.cmxs  -package lutin,lustre-v6 -linkpkg  $*.ml 

clean:
	rm -rf *.ec *.cm* *.log *~ .*~ *.o *rif0 *rif Data  *.res *ession*ml my-rdbg-tuning.ml

EXPDIR=$(shell $(OCAMLOPT) -version | cut -d " " -f5)

$(EXPDIR):
	[ -d $(EXPDIR) ] || (mkdir -p $(EXPDIR); make utest)


test1.rif:
	rdbg -lurette -o $@ \
        -sut "lv6 led.lus -n puzzle1" \
        -env "lutin env.lut -n main -seed 42"\
        -oracle "lv6 led.lus -n puzzle1_has_no_solution"

test1: $(EXPDIR) test1.rif $(EXPDIR)/test1.rif.exp
	diff -B -u 0 -i  $(EXPDIR)/test1.rif.exp test1.rif \
		| grep -v "Version" > test1.res || true
	cat test1.res
	[ ! -s test1.res ]

utest1: $(EXPDIR)  test1.rif
	cp test1.rif $(EXPDIR)/test1.rif.exp

####################################################################################
# The following works if you have installed the binary dsitribution

test-bin-dist:
	rdbg -lurette -o $@.rif \
        -sut "lv6 led.lus -n puzzle1" \
        -env "lutin env.lut -n main -seed 42"\
        -oracle "lv6 led.lus -n puzzle1_has_no_solution"
	$(OCAMLOPT) -shared -o luretteSession.cmxs \
       $(V6TOOLS_LIBPATH) $(V6TOOLS_CMXA)  luretteSession.ml
	rdbg-batch luretteSession.cmxs  && echo "ok" || echo "ko"
	cat test-bin-dist.rif



