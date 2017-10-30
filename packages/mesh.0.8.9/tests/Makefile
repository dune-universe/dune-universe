OCAMLMAKEFILE = ../OCamlMakefile
ANNOTATE := yes
OCAMLNCFLAGS = -inline 10

INCDIRS += ../src
LIBS += mesh mesh_display
PACKS = lacaml,graphics

SOURCES = square.ml
RESULT = square

all: opt
byte: byte-code
opt: native-code

include $(OCAMLMAKEFILE)
