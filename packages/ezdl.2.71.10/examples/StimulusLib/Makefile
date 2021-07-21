
EXE=
ifeq ($(HOSTTYPE),win32)
  EXE=.exe
endif

ifndef LUTIN
  LUTIN=lutin
endif

EXPDIR=`$(LUTIN) --ocaml-version`
$(EXPDIR):
	[ -d $(EXPDIR) ] || (mkdir -p $(EXPDIR) ; make utest)

test.rif : 
	lurette -l 100 -o test.rif0 \
			  -sut "lutin -seed 1 temporal.lut -main main" \
           -env "lutin -seed 1 temporal.lut -main env" 
	cat test.rif0 | sed -e "s/^M//"  | grep -v " Version"  > test.rif

test:test.rif  $(EXPDIR)
	rm -f test.res && diff -B -u -i -w  $(EXPDIR)/test.rif.exp test.rif > test.res
	[ ! -s test.res ] && make clean 

utest:test.rif
	cp test.rif $(EXPDIR)/test.rif.exp ; true


clean:
	rm -rf $(OBJDIR) rm -rf *.res *.log *~ *.rif0 *.rif *.gp *.plot




