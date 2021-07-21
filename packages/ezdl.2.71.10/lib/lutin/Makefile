
# cluctch to fix the unability of dune to deal with .idl files

CAMLIDL=camlidl

IDLFILES =$(shell ls *.idl)
CFILE=$(IDLFILES:%.idl=%_stubs.c)
MLFILE=$(IDLFILES:%.idl=%.ml)
MLIFILE=$(IDLFILES:%.idl=%.mli)

all:  $(CFILE) $(MLFILE) $(MLIFILE)
idl : $(IDLFILES)

# camlidl
%_stubs.c %.ml %.mli: %.idl 
	$(CAMLIDL) -nocpp -I . $*.idl

clean:
	/bin/rm -f $(CFILE) $(MLFILE) $(MLIFILE)

