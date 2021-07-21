

EXE=''
ifneq (,$(findstring $(HOSTTYPE),win32))
	LD_ARCH=-DWIN32
	EXE=".exe"
endif
ifeq ($(HOSTTYPE),cygwin)
	LD_ARCH=-DWIN32
endif
ifeq ($(HOSTTYPE),mac)
	LD_ARCH=-DMAC
endif

# XXX pack all those modules into one
CMXA_LIB = dynlink.cmxa unix.cmxa nums.cmxa str.cmxa \
           ezdl.cmxa gbddml.cmxa polka.cmxa \
			  lutils.cmxa rdbg.cmxa lutinRun.cmxa 

CLIB= `ocamlfind query -r rdbg -i-format` \
		`ocamlfind query -r lutin -i-format` \
		-cclib -lcamlidl -cclib -lgmp
INCLUDE_DIR = -I +gmp -I +camlidl -I +polka -I +gbddml \
              -I +rdbg -I +lutin  -I +ezdl 
LUTIN=lutin$(EXE)


################################################################

MAIN=call_foo

$(MAIN).opt:$(MAIN).ml
	ocamlfind ocamlopt -cc g++ $(CLIB) $(INCLUDE_DIR)  $(CMXA_LIB)  $(MAIN).ml -o $(MAIN).opt

$(MAIN).opt_alt:$(MAIN).ml
	ocamlfind ocamlopt -package rdbg,lutin  $(MAIN).ml   -o $(MAIN).opt


$(MAIN):$(MAIN).ml
	ocamlfind ocamlc.opt $(INCLUDE_DIR) graphics.cma $(CMA_LIB)  $(MAIN).ml -o $(MAIN)



################################################################


clean:
	rm -f *.top *.cmi *.cmo *.cmx *.o *.cma *.cmxa *.a *.cmi *.cmo *.cmi *.cmx *.o  *~ $(MAIN) $(MAIN).opt *~ *.pp_luc *.rif test.res *.gp *.plot

EXPDIR=`$(LUTIN) --ocaml-version`
$(EXPDIR):
	[ -d $(EXPDIR) ] || (mkdir -p $(EXPDIR) ; make utest)


$(MAIN).rif: clean $(MAIN).opt $(EXPDIR)
	./$(MAIN).opt

test:$(MAIN).rif
	rm -f test.res && diff -B -u -i $(MAIN).rif.exp $(MAIN).rif > test.res && \
	[ ! -s test.res ] && make clean 

utest:$(MAIN).rif
	cp $(MAIN).rif $(MAIN).rif.exp



