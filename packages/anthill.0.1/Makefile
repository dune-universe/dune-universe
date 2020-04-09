all: anthill

anthill:
	dune build bin/main.exe
	cp _build/default/bin/main.exe anthill

clean:
	ocamlbuild -clean
	rm anthill
