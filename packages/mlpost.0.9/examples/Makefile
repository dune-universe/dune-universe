MAX := 17
# all files that are generated from boxes.ml
BOXESPNG := $(foreach i,$(shell seq 1 9), boxes$(i).png)

# all files that are generated from paths.ml
PATHSPNG := $(foreach i,$(shell seq 1 17), paths$(i).png)

# all files that are generated from misc.ml
MISCPNG := $(foreach i,1 2 $(shell seq 4 9) $(shell seq 11 14), misc$(i).png)

# all files that are generated from tree.ml
TREEPNG := $(foreach i,$(shell seq 1 14), tree$(i).png)


# all files that are generated from label.ml
LABELPNG := $(foreach i,$(shell seq 1 2), label$(i).png)

# all files that are generated from automata.ml
AUTOMPNG := automata1.png automata2.png automata4.png

HISTPNG := hist1.png

RADARPNG := radar1.png radar2.png

REALPLOTPNG := real_plot1.png real_plot2.png

COLORPNG := color1.png color2.png color3.png color4.png

INCLUDEPNG := include1.png include2.png include3.png

DOTPNG := dot_dot1.png dot_dot2.png

HTMLFILES := boxes.ml.html paths.ml.html misc.ml.html tree.ml.html \
             label.ml.html automata.ml.html hist.ml.html radar.ml.html\
	     real_plot.ml.html dot_dot.ml.html color.ml.html include.ml.html

MLPOST:=mlpost -v -ps

ALL := $(BOXESPNG) $(PATHSPNG) $(MISCPNG) $(TREEPNG) $(LABELPNG) $(AUTOMPNG)\
	$(HISTPNG)  $(RADARPNG) $(REALPLOTPNG) $(COLORPNG) $(INCLUDEPNG)

#Direct mps output
ALL_MPS :=  $(addprefix mps_,$(ALL))

ALL_CAIRO := $(addprefix cairo_,$(ALL))

ALL_CAIRO_PNG := $(addprefix png_,$(ALL_CAIRO))

ALL_CAIRO_PDF := $(addprefix pdf_,$(ALL_CAIRO))

ALL_CAIRO_PS := $(addprefix ps_,$(ALL_CAIRO))
ALL_CAIRO_SVG := $(addprefix svg_,$(ALL_CAIRO:.png=.svg))

all : all_metapost all_cairo all_mps

all_metapost: $(ALL)
all_mps: $(ALL_MPS)

all_cairo: all_cairo_png all_cairo_ps all_cairo_pdf all_cairo_svg

all_cairo_png:  $(ALL_CAIRO_PNG)
all_cairo_pdf:  $(ALL_CAIRO_PDF)
all_cairo_ps:  $(ALL_CAIRO_PS)
all_cairo_svg:  $(ALL_CAIRO_SVG)

.PHONY: all all_metapost all_mps all_cairo all_cairo_png all_cairo_pdf \
	all_cairo_ps all_cairo_svg

contrib:$(DOTPNG)\
	$(addprefix png_cairo_,$(DOTPNG))\
	$(addprefix pdf_cairo_,$(DOTPNG))\
	$(addprefix ps_cairo_,$(DOTPNG))\
	$(addprefix svg_cairo_,$(DOTPNG:png=svg))

#Compilation
%_dot.native : %_dot.ml
	$(MLPOST) -native -contrib dot -dont-execute -compile-name $@ $^

%.native : %.ml
	$(MLPOST) -native -dont-execute -compile-name $@ $^

#With Metapost :
$(foreach i,$(shell seq 1 $(MAX)), %$(i).1) : %.native
	./$^ -ps

#With direct mps output :
$(foreach i,$(shell seq 1 $(MAX)), mps_%$(i).mps) : %.native
	./$^ -mps -prefix "mps_"

#With cairo pdf
$(foreach i,$(shell seq 1 $(MAX)), pdf_cairo_%$(i).pdf) : %.native
	./$^ -pdf -cairo -prefix "pdf_cairo_"

#With cairo ps
$(foreach i,$(shell seq 1 $(MAX)), ps_cairo_%$(i).ps) : %.native
	./$^ -ps -cairo -prefix "ps_cairo_"

#With cairo png
$(foreach i,$(shell seq 1 $(MAX)), png_cairo_%$(i).png) : %.native
	./$^ -png -cairo -prefix "png_cairo_"

#With cairo svg
$(foreach i,$(shell seq 1 $(MAX)), svg_cairo_%$(i).svg) : %.native
	./$^ -svg -cairo -prefix "svg_cairo_"

parse.ml: parse.mll
	ocamllex parse.mll

parse: parse.ml
	ocamlopt.opt -o parse parse.ml

#Other
html: $(HTMLFILES)
%.ml.html : %.ml parse style.css
	caml2html -css -hc -ext "parse:./parse" $*.ml


%.png: %.ps
	convert $^  $@

%.png: %.pdf
	convert $^  $@

%.pdf.tex: all.pdf.template
	sed -e 's/all/$*/' all.pdf.template > $@

%.tex: all.template
	sed -e 's/all/$*/' all.template > $@

%.ps: %.1 %.tex
	latex $*
	dvips -E $*.dvi -o

%.pdf: %.mps %.pdf.tex
	pdflatex $*.pdf.tex
	mv $*.pdf.pdf $*.pdf

# %.pdf: %.mps all.template2
# 	sed -e 's/all/$*/' all.template2 > $*.tex
# 	pdflatex $*

# %.mps: %.1
# 	cp $*.1 $*.mps

ALLTEX:=$(ALL:.png=.tex) $(ALL_MPS:.png=.pdf.tex)

clean:
	rm -f *.aux *.dvi *.ps *.1 *.log $(PNGFILES) *.mp *.mpx *.mps *.pdf
	rm -f $(ALLTEX)
	rm -f $(HTML)
	rm -f parse.ml *.cmx *.cmo *.cmi parse *.o
	rm -f *.dummy *.dummy_dot *.native *.annot
	rm -f $(filter-out powered-by-caml.128x58.png,$(wildcard *.png))


editor2 :
	ocamlbuild editor2.native

lattice_lablgtk : lattice_lablgtk.ml
	$(MLPOST) -contrib lablgtk lattice_lablgtk.ml
