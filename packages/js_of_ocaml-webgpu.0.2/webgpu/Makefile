OCAMLC_FLAGS = -w +a-4-40-41-42-44-45-48 

SRC=$(shell find -name "*.ml" -or -name "*.mli")

all: webgpu.a

# cp _build/default/print.exe print
# cp _build/default/gen.exe gen

webgpu.a: ${SRC}
	dune build webgpu.a

clean:
	dune clean
	rm $(EXE)
FORCE:
