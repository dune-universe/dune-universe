##
##  Makefile for Standard, Profile, Debug, and Release version of MiniSat
##

CSRCS     = $(wildcard *.c)
CHDRS     = $(wildcard *.h)
COBJS     = $(addsuffix .o, $(basename $(CSRCS)))

PCOBJS    = $(addsuffix p,  $(COBJS))
DCOBJS    = $(addsuffix d,  $(COBJS))
RCOBJS    = $(addsuffix r,  $(COBJS))

EXEC      = minisat

CC        = gcc
CFLAGS    = -std=c99
COPTIMIZE = -O3 -fomit-frame-pointer


.PHONY : s p d r build clean depend lib libd

s:	WAY=standard
p:	WAY=profile
d:	WAY=debug
r:	WAY=release
rs:	WAY=release static

s:	CFLAGS+=$(COPTIMIZE) -ggdb -D NDEBUG
p:	CFLAGS+=$(COPTIMIZE) -pg -ggdb -D DEBUG
d:	CFLAGS+=-O0 -ggdb -D DEBUG
r:	CFLAGS+=$(COPTIMIZE) -D NDEBUG
rs:	CFLAGS+=$(COPTIMIZE) -D NDEBUG

s:	build $(EXEC)
p:	build $(EXEC)_profile
d:	build $(EXEC)_debug
r:	build $(EXEC)_release
rs:	build $(EXEC)_static

build:
	@echo Building $(EXEC) "("$(WAY)")"

clean:
	@rm -f $(EXEC) $(EXEC)_profile $(EXEC)_debug $(EXEC)_release $(EXEC)_static \
	  $(COBJS) $(PCOBJS) $(DCOBJS) $(RCOBJS) depend.mak

## Build rule
%.o %.op %.od %.or:	%.c
	@echo Compiling: $<
	@$(CC) $(CFLAGS) -c -o $@ $<

## Linking rules (standard/profile/debug/release)
$(EXEC): $(COBJS)
	@echo Linking $(EXEC)
	@$(CC) $(COBJS) -lz -lm -ggdb -Wall -o $@ 

$(EXEC)_profile: $(PCOBJS)
	@echo Linking $@
	@$(CC) $(PCOBJS) -lz -lm -ggdb -Wall -pg -o $@

$(EXEC)_debug:	$(DCOBJS)
	@echo Linking $@
	@$(CC) $(DCOBJS) -lz -lm -ggdb -Wall -o $@

$(EXEC)_release: $(RCOBJS)
	@echo Linking $@
	@$(CC) $(RCOBJS) -lz -lm -Wall -o $@

$(EXEC)_static: $(RCOBJS)
	@echo Linking $@
	@$(CC) --static $(RCOBJS) -lz -lm -Wall -o $@

lib:	libminisat.a
libd:	libminisatd.a

libminisat.a:	solver.or csolver.or
	@echo Library: "$@ ( $^ )"
	@rm -f $@
	@ar cq $@ $^

libminisatd.a:	solver.od csolver.od
	@echo Library: "$@ ( $^ )"
	@rm -f $@
	@ar cq $@ $^


## Make dependencies
depend:	depend.mak
depend.mak:	$(CSRCS) $(CHDRS)
	@echo Making dependencies ...
	@$(CC) -MM $(CSRCS) > depend.mak
	@cp depend.mak /tmp/depend.mak.tmp
	@sed "s/o:/op:/" /tmp/depend.mak.tmp >> depend.mak
	@sed "s/o:/od:/" /tmp/depend.mak.tmp >> depend.mak
	@sed "s/o:/or:/" /tmp/depend.mak.tmp >> depend.mak
	@rm /tmp/depend.mak.tmp

include depend.mak
