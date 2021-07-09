# test rdbg (the rdbg-top wrapper)

RDBG=rdbg
RDBGTOP=rdbg-top
ifndef OCAMLOPT
export OCAMLOPT=ocamlfind ocamlopt
endif
EXPDIR=$(shell $(OCAMLOPT) -version | cut -d " " -f5)


test: test1 test2 clean

$(EXPDIR):
	[ -d $(EXPDIR) ] || (mkdir -p $(EXPDIR) ; make utest)


.PRECIOUS: rdbg-session.ml

# The first test generates a rdbg-session.ml file, and exercise it
# using test1-session
test1-session.out:
	$(RDBG) -go -in test1-session -sut "lv6 chrono_v4.lus -n chrono_v4" -env "lutin chrono_env.lut -n chrono_real_env -seed 1" && \
	grep -v "Version"  session.rif > $@



rdbg:
	$(RDBG) -sut "lv6 chrono_v4.lus -n chrono_v4" \
           -env "lutin chrono_env.lut -n chrono_real_env -seed 1" -o rdbg.rif
lurette:
	$(RDBG) -lurette \
	   -sut "lv6 chrono_v4.lus -n chrono_v4" \
           -env "lutin chrono_env.lut -n chrono_real_env -seed 1" -o lurette.rif

test1: $(EXPDIR) test1-session.out $(EXPDIR)/test1.exp
	rm -f test1.res
	diff -B -u -i $(EXPDIR)/test1.exp test1-session.out | \
		grep -v "Version" > test1.res || true
	cat test1.res
	[ ! -s test1.res ]

utest1: test1-session.out
	cp $< $(EXPDIR)/test1.exp



# The second test reuse the rdbg-session.ml generated in test1 and
# check the ability of rdbg to execute ocaml script
test2-session.out: rdbg-session.ml
	echo "quit" | $(RDBGTOP) test2-session.ml   | \
	grep -v "Version"  > $@

test2: $(EXPDIR) test2-session.out $(EXPDIR)/test2.exp
	rm -f test2.res
	diff -B -u -i $(EXPDIR)/test2.exp test2-session.out > test2.res || true
	cat test2.res
	[ ! -s test2.res ]

utest2: test2-session.out
	cp $< $(EXPDIR)/test2.exp



clean:
	rm -f rdbg-session*.ml rdbg.rif test1-session.out test2-session.out *.res
	rm -f *.pdf *.ps *.dot *.dro *.log *.c

.PHONY:test1 test2
test: test1 test2 clean 
utest: utest1 utest2

