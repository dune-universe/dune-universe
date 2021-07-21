
EXE=
ifeq ($(HOSTTYPE),win32)
  EXE=.exe
endif

ifndef LUTIN
  LUTIN=../../_build/default/bin/lutin/main.exe
endif

EXPDIR=`$(LUTIN) --ocaml-version`
$(EXPDIR):
	[ -d $(EXPDIR) ] || (mkdir -p $(EXPDIR) ; make utest)

test.rif : 
	$(LUTIN) -seed 1 -l 100 ud.lut -main main  | sed -e "s/^M//"  | grep -v " Version"  > test.rif

test:test.rif  $(EXPDIR)
	rm -f test.res && diff -B -u -i -w  $(EXPDIR)/test.rif.exp test.rif > test.res
	[ ! -s test.res ] && make clean 

utest:test.rif
	cp test.rif $(EXPDIR)/test.rif.exp ; true


clean:
	rm -rf $(OBJDIR) rm -rf *.res *.log *~ *.rif0 *.rif *.gp *.plot *.dro *.dot




