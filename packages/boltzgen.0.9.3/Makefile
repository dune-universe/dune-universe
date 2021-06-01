dune:
	dune build @install
	ln -sf _build/default/gen_test.exe boltzgen

lib:
	ocamlbuild -use-ocamlfind -I exam gen_test_lib.cma


all:
	ocamlbuild -use-ocamlfind -I exam gen_test_lib.cma question_bank.cmo gen_subject.byte load_xlsx.byte gen_tex_from_spreadsheet.byte

minimal:
	ocamlbuild -use-ocamlfind gen_test_lib.cma


allall:
	ocamlbuild -use-ocamlfind -I exam gen_test.byte gen_test_lib.cma question_bank.cmo gen_subject.byte load_xlsx.byte gen_tex_from_spreadsheet.byte

gen: all
	./question_bank.byte bpicsou
	pdflatex qout.tex

web:
	rm -rf website
	dune build web/gen_test_web.js
	mkdir -p website
	cp web/index.html website/index.html
	cp web/style.css website/style.css
	cp _build/default/web/gen_test_web.js website/gen_test_web.js

web2:
	rm -rf website
	ocamlbuild -I web -use-ocamlfind gen_test_web.byte gen_test_lib.cma 
	js_of_ocaml gen_test_web.byte
	mkdir website
	mv gen_test_web.js website/
	cp web/style.css website/
	cp web/index.html website/
	cp _build/gen_test_lib.cma website/

clean:
	ocamlbuild -clean
	rm -rf website
	dune clean

.PHONY: clean dune all web
