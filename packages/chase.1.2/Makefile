# OCaml Makefile that relies on ocamlbuild
#
# Type "make chase.d.otarget" to make a debugable executable
#
# Enable warnings by adding to OCFLAGS: -cflags "-w +A-4"

OCFLAGS	= -classic-display -use-ocamlfind -cflags "-w +A-4"
OCB	= ocamlbuild
# For use with the debugger, define DEBUG as follows
# DEBUG	= d.
# For profiling, define DEBUG as follows
# DEBUG	= p.
TARGET	= chase.$(DEBUG)otarget

all:
	$(OCB) $(OCFLAGS) $(TARGET)

clean:
	$(OCB) $(OCFLAGS) -clean

Makefile:
	@echo make $@

%:	force
	$(OCB) $(OCFLAGS) $@

.PHONY:	all clean force
