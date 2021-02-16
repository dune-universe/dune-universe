all: slides.pdf

FIGURES:= fibtree.pdf

$(FIGURES): figures.ml
	mlpost -cairo $<
	make -C ../../papers/jfla2009/ figures.mp

view: slides.pdf
	xpdf slides.pdf

slides.pdf: $(FIGURES) slides.tex

%.pdf: %.tex
	pdflatex $<
