# to run the test, one needs to (apt-get) install:
# dejagnu tcl tcllib

all:  clean  qtest rest
test: clean  qtest
rest: reftest lus2lic.time 
jenkins: clean qtest rest


TEST_MACHINE=ssh ovaz
TEST_MACHINE=eval
# This fixes sporadic memory errors I get when running tests
OCAMLRUNPARAM=s=1M,i=32M,o=150



copy_extern:
	cp -f extern_files/* .

testdir=$(shell pwd)

remote-runtest: 
#	runtest --all --tool lus2lic --target_board=$(testdir)/board_$(TEST_MACHINE).exp  || true
	$(TEST_MACHINE) "cd  $(testdir) ; export OCAMLRUNPARAM=o=200 ; runtest --all --tool lus2lic"  || true
	cd $(testdir)

LOG=lus2lic0.log lus2lic1.log lus2lic2.log lus2lic3.log lus2lic4.log
SUM=lus2lic0.sum lus2lic1.sum lus2lic2.sum lus2lic3.sum lus2lic4.sum

lus2lic.sum:
	head -n -5 $(SUM) | sed -e 's/[0-9][0-9][0-9][0-9][0-9]//' | sed -e 's/[0-9][0-9][0-9][0-9]//' > lus2lic.sum
	tail -n 5  $(SUM)  >>  lus2lic.sum
	echo -n "===============================\n# Total number of failures: " >>  lus2lic.sum 
	cat lus2lic*.log | grep "FAIL:" | grep -v "XFAIL" | wc -l >>  lus2lic.sum 

.PHONY:lus2lic.diff lus2lic.time lus2lic.sum
lus2lic.diff:
	cat lus2lic.log | grep  -v "Process " | grep -v "Killing process " | \
		grep -v "simec_trap " | grep -v " Version "  > lus2lic2.log
	mv lus2lic2.log lus2lic.log
	diff -U 1 -I "Test Run By" -I "testcase \./lus2lic" -I "runtest completed at" \
	 -I "This is Lurette Version "  \
	lus2lic.log.ref lus2lic.log  > lus2lic.diff || \
	(cat lus2lic.diff; echo "make utest if it is fine"; exit 1)

time:lus2lic.time
lus2lic.time:lus2lic.sum
	grep "testcase \./lus2lic" lus2lic*.log > lus2lic.time
	echo "* Ref time: " >> lus2lic.time
	cat lus2lic.ref_time >> lus2lic.time || true 
	echo "* Quick time (-j 4):" >> lus2lic.time
	cat lus2lic.quick_time >> lus2lic.time || true
	cat lus2lic.time >> lus2lic.sum
	cat lus2lic.time
.PHONY:lus2lic.time

utest:
	cp lus2lic.log lus2lic.log.ref


# reftest isslower than qtest, but less sensible to the load of the machine (wrt time measurements)
reftest: 
	rm -f $(LOG)
	$(TEST_MACHINE) "cd $(testdir); make tmpdirs"
	time -o lus2lic.ref_time make $(LOG)
	make lus2lic.time

qtest: 
	rm -f $(LOG)
	$(TEST_MACHINE) "cd $(testdir); make tmpdirs"
	time -o lus2lic.quick_time make -j 4 $(LOG)
	cat lus2lic.quick_time
	make lus2lic.time

ltmpdirs:
	[ -d /tmp/lus2lic-test ] || mkdir /tmp/lus2lic-test
	[ -d /tmp/lus2lic-test-tmp1 ] || mkdir /tmp/lus2lic-test-tmp1
	[ -d /tmp/lus2lic-test-tmp2 ] || mkdir /tmp/lus2lic-test-tmp2

tmpdirs: ltmpdirs
	[ -d tmp ] || mkdir tmp
	[ -d tmp1 ] || mkdir tmp1
	[ -d tmp2 ] || mkdir tmp2
	rm -rf tmp/*
	rm -rf tmp1/*
	rm -rf tmp2/*
	cp -f --preserve=links $(testdir)/* tmp/ || true
	cp -f --preserve=links $(testdir)/* tmp1/ || true
	cp -f --preserve=links $(testdir)/* tmp2/ || true

test0: rmlus2lic0.log lus2lic0.log
rmlus2lic0.log: 
	rm -f lus2lic0.log

test1: rmlus2lic1.log lus2lic1.log
rmlus2lic1.log: 
	rm -f lus2lic1.log

test2: rmlus2lic2.log lus2lic2.log
rmlus2lic2.log: 
	rm -f lus2lic2.log

test3: rmlus2lic3.log lus2lic3.log
rmlus2lic3.log: 
	rm -f lus2lic3.log

test4: rmlus2lic4.log lus2lic4.log
rmlus2lic4.log: 
	rm -f lus2lic4.log

unexp:
	grep unexp lus2lic.sum

lus2lic0.log:
	$(TEST_MACHINE) "cd $(testdir); runtest --tool lus2lic0 lus2lic.tests/test0.exp" || true

lus2lic1.log:
	$(TEST_MACHINE) "cd $(testdir); runtest --tool lus2lic1 lus2lic.tests/test1.exp" || true

lus2lic2.log:
	$(TEST_MACHINE) "cd $(testdir); runtest --tool lus2lic2 lus2lic.tests/test2.exp" || true

lus2lic3.log:
	$(TEST_MACHINE) "cd $(testdir); runtest --tool lus2lic3 lus2lic.tests/test3.exp" || true

lus2lic4.log:
	$(TEST_MACHINE) "cd $(testdir); runtest --tool lus2lic4 lus2lic.tests/test4.exp" || true


kcg.log:
	$(TEST_MACHINE) "cd $(testdir); runtest --tool lus2lic-kcg lus2lic.tests/kcg.exp" || true

kcg: kcg.log

progression:
	$(TEST_MACHINE) "cd $(testdir); runtest --tool lus2lic lus2lic.tests/progression.exp" || true



local_clean: lclean tmpdirs
clean:
	cd $(testdir); make local_clean
lclean: 
	rm -f *.ec *.lus *.lut *.cov *.gp *.rif *.out *.cov *.c *.h *.log *.sum *time
	rm -rf Data



