
ifndef LUTINSRC
  LUTINSRC=~/lurette/lib/lutin
endif
ifndef LUT2TEX
  LUT2TEX=~/rdbg/utils/lutintolatex
endif

SRCS=./commands.tex \
     ./language.tex \
	 ./objs/lutin.tex ./objs/gnuplotrif.tex  ./objs/checkrif.tex \
	./objs/foo.c.tex \
	./objs/call_external_c_code.lut.tex \
	./objs/polar.lut.tex \
	./objs/ud.lut.tex \
	./objs/rabbit.lut.tex \
	./objs/ud.jpg \
     ./lutsyntax.tex \
     ./semantics.tex \
     ./lutin_toplevel.tex \
     ./lutin_examples.tex \
     ./objs/lutyacc.tex \
     ./objs/version.tex \
      ./main.tex touch.tex


all: touch.tex lutin-man.pdf 
re:lutin-man.pdf touch.tex 
	make 

.PHONY:lutin-man.pdf objs/main.pdf
lutin-man.pdf: objs/main.pdf
	cp $< $@

./objs/lutin.tex: objs
	lutin$(EXE) -help > $@

# XXX ecrire un filtre pour rajouter de la coloration syntaxique !!
./objs/foo.c.tex:
	cat ../../examples/external_code/foo.c > ./objs/foo.c.tex

# XXX idem
./objs/call_external_c_code.lut.tex:../../examples/external_code/call_external_c_code.lut
	$(LUT2TEX) ../../examples/external_*sccode/call_external_c_code.lut > ./objs/call_external_c_code.lut.tex

D=../../examples/up_and_down/$(shell ocamlc -version)
objs/ud.rif: $(D)/test.rif.exp
	cp $< $@

./objs/ud.jpg: objs/ud.rif
	cd objs ; gnuplot-rif$(EXE) -jpg ud.rif 



./objs/polar.lut.tex:
	$(LUT2TEX) ../../examples/external_code/polar.lut > ./objs/polar.lut.tex

./objs/ud.lut.tex:
	$(LUT2TEX) ../../examples/up_and_down/ud.lut > ./objs/ud.lut.tex

./objs/rabbit.lut.tex:
	$(LUT2TEX) ../../examples/crazy-rabbit/rabbit.lut > ./objs/rabbit.lut.tex


./objs/gnuplotrif.tex:objs
	gnuplot-rif$(EXE) --help 1> $@

./objs/checkrif.tex:objs
	check-rif$(EXE) --help 1> $@


touch.tex: 
	touch touch.tex

objs/main.pdf: objs $(SRCS)
	cp main.tex objs; 
	cd objs; pdflatex main.tex; bibtex main; pdflatex main.tex

objs:
	mkdir objs

BIN=/usr/local/tools/lustre/misc/bin

./objs/lutyacc.tex: $(LUTINSRC)/lutParser.mly
	$(BIN)/yacc2latex -t tokens $(LUTINSRC)/lutParser.mly -o $@ 

./objs/cleanyacc : $(LUTINSRC)/lutParser.mly
	cat $< | sed -e "s/lut//g" -e "s/TK_EOF//" > $@ 

.PHONY:objs/version.tex
./objs/version.tex: 
	cp ../version.tex objs

clean:
	rm -f ./objs/*

www:
	cp lutin-man.pdf  $(SYNCHRONE_DIR)/lurette/
