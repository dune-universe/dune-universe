#-----------------------------------------------------
# Dynamic linking of C code with Lucky
# 
#-----------------------------------------------------

# Directories
SRCDIR=.
OBJDIR=./obj

MAIN=foo
MAIN2=bool_utils_au_plus_2_parmi_3


#settings for Linux (and other posix systems ?)
DLEXT=so
EXE=
LD=gcc -fPIC
CC=gcc -fPIC
MKDLLFLAGS=
LFLAGS=
SOFLAGS=-shared


#settings for WIN32
ifeq ($(HOSTTYPE),win32)
	DLEXT=dll
	EXE=.exe
	LD=gcc -DWIN32 -mno-cygwin
	CC=gcc -DWIN32 -mno-cygwin
	MKDLLFLAGS=-Wl,--out-implib,$(OBJDIR)/$(MAIN).a
	LFLAGS=-lm
	LD_ARCH=-DWIN32
endif
ifeq ($(HOSTTYPE),cygwin)
	DLEXT=dll
	EXE=.exe
	LD=gcc -DWIN32 
	CC=gcc -DWIN32 
	MKDLLFLAGS=-Wl,--out-implib,$(OBJDIR)/$(MAIN).a
	MKDLL2FLAGS=-Wl,--out-implib,$(OBJDIR)/$(MAIN2).a
	LFLAGS=-lm
	LD_ARCH=-DWIN32
endif

ifeq ($(HOSTTYPE),mac)
	LD=g++
	CC=gcc
	SOFLAGS = -dynamiclib 
	LD_ARCH=-DMAC
endif

all: $(OBJDIR)/$(MAIN).$(DLEXT)  $(OBJDIR)/$(MAIN2).$(DLEXT) 

$(OBJDIR):
	mkdir $(OBJDIR)

$(OBJDIR)/$(MAIN).$(DLEXT) : $(OBJDIR)/$(MAIN).o
	$(LD) $(LFLAGS) $(SOFLAGS) -o $(OBJDIR)/$(MAIN).$(DLEXT) \
         $(OBJDIR)/$(MAIN).o \
			$(MKDLLFLAGS)

$(OBJDIR)/$(MAIN2).$(DLEXT) : $(OBJDIR)/$(MAIN2).o
	$(LD) $(LFLAGS) $(SOFLAGS) -o $(OBJDIR)/$(MAIN2).$(DLEXT) \
         $(OBJDIR)/$(MAIN2).o \
			$(MKDLLFLAGS2)

$(OBJDIR)/%.o : $(OBJDIR) $(SRCDIR)/%.c
	$(CC) -c $(CFLAGS) $(SRCDIR)/$*.c -o $(OBJDIR)/$*.o

$(OBJDIR)/bool_utils_au_plus_2_parmi_3.ec : $(SRCDIR)/bool-utils.lus
	lus2lic -ec $< -n bool_utils_au_plus_2_parmi_3 -o $@

$(OBJDIR)/bool_utils_au_plus_2_parmi_3.c : $(OBJDIR)/bool_utils_au_plus_2_parmi_3.ec
	cd $(OBJDIR) ; ec2c ../$<


ifndef LUTIN
  LUTIN=../../_build/default/bin/lutin/main.exe
endif

run: $(OBJDIR)/$(MAIN).$(DLEXT) 
	$(LUTIN) call_external_c_code.lut -m Fun_Call -L libm.so -L obj/foo.so \
		 -l 10 

EXPDIR=`$(LUTIN) --ocaml-version`
$(EXPDIR):
	[ -d $(EXPDIR) ] || (mkdir -p $(EXPDIR) ; make utest)



test1.rif: clean $(OBJDIR)/$(MAIN).$(DLEXT) $(EXPDIR)
	$(LUTIN) call_external_c_code.lut -m Fun_Call -L libm.so -L obj/foo.so \
	 -l 10  -seed 834966010 | sed -e "s/^M//"  | grep -v " Version"  > test1.rif

test1:test1.rif
	rm -f test1.res && diff -u -i -B $(EXPDIR)/test1.rif.exp test1.rif   > test1.res  || true
	cat test1.res
	[ ! -s test1.res ] && make clean 

test2.rif:$(EXPDIR)
	echo "1.0 1.0" | $(LUTIN)  -l 1 -L libm.so polar.lut -m cartesian \
	                  -seed 1 | grep -v " Version"  > test2.rif

test2:test2.rif
	echo "1.0 1.0" | $(LUTIN)  -l 1 -L libm.so polar.lut -m cartesian \
	                  -seed 1 | grep -v " Version"  > test2.rif
	rm -f test2.res && diff -u -i -B $(EXPDIR)/test2.rif.exp test2.rif   > test2.res || true
	cat test2.res
	[ ! -s test2.res ] && make clean 

test3.rif: $(OBJDIR) $(OBJDIR)/$(MAIN2).$(DLEXT) $(EXPDIR)
	echo "1.0 1.0" | $(LUTIN)  -l 1 -L $(OBJDIR)/$(MAIN2).$(DLEXT) call_lustre.lut  \
	                  -seed 1 | grep -v " Version"  > test3.rif
test3: test3.rif
	echo "1.0 1.0" | $(LUTIN)  -l 1 -L $(OBJDIR)/$(MAIN2).$(DLEXT) call_lustre.lut  \
	                  -seed 1 | grep -v " Version"  > test3.rif
	rm -f test3.res && diff -u -i -B $(EXPDIR)/test3.rif.exp test3.rif   > test3.res || true
	cat test3.res
	[ ! -s test3.res ] && make clean 


test: test1 test2 


utest1: test1.rif
	cp test1.rif $(EXPDIR)/test1.rif.exp
utest2:test2.rif
	cp test2.rif $(EXPDIR)/test2.rif.exp

utest3:test3.rif
	cp test3.rif $(EXPDIR)/test3.rif.exp

utest: utest1 utest2

clean:
	rm -rf $(OBJDIR) *.rif *.rif0 *.res *.gp *.plot *.tex



