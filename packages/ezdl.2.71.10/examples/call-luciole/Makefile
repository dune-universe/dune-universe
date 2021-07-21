

LURETTE=lurette  \
	  -sut "lus2lic heater_control.lus -n heater_control"  \
	  -oracle "lus2lic heater_control.lus -n not_a_sauna"  \
	  -env "lutin env.lut -n main -seed 3 -p 2" 

EXPDIR=`rdbg --ocaml-version`
$(EXPDIR):
	[ -d $(EXPDIR) ] || (mkdir -p $(EXPDIR) ; make utest)


test.rif:
	rm -f test.rif .lurette_rc
	export GCC="/usr/bin/gcc -fPIC"
	$(LURETTE) --luciole --test-length 2  --output test.rif0 && \
	grep -v "lurette chronogram" test.rif0  | \
	grep -v "Version"  | grep -v "#seed " | \
	grep -v "The execution lasted"| sed -e "s/^M//" > test.rif

test:test.rif $(EXPDIR)
	rm -f test.res
	diff -B -u -i  $(EXPDIR)/test.rif.exp test.rif > test.res 
	cat test.res
	[ ! -s test.res ] && make clean
	make clean 

utest:
	cp test.rif $(EXPDIR)/test.rif.exp

clean:
	rm -rf *.ec *.log *~ .*~ *.o *rif0 *rif Data *.pp_luc *.plot *.gp *.dro  *_luciole.c test.res luretteSession*

