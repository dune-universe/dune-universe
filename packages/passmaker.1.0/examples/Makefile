all: gen.native

gen.native: gen.ml
	ocamlfind ocamlopt -package passmaker -linkpkg -o $@ $<

clean:
	rm -f gen.native gen.o gen.cm[ix]
