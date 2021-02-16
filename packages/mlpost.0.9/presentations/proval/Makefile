MPSFILES := cfg proval mpex colortriangle path cheno pictures tree2 \
		tree6 florence
MPSFILES := $(patsubst %,%.mps, $(MPSFILES))

GENERATED := cfg proval colortriangle path cheno pictures tree2 tree6
GENERATED := $(patsubst %,%.mp, $(GENERATED))

pdf: slides.pdf

slides.pdf: $(MPSFILES) slides.tex
	pdflatex slides.tex
	pdflatex slides.tex

%.mps: %.ml
	mlpost -pdf -latex slides.tex $^

clean:
	rm -f *.toc *.aux *.pdf *.log *~ *.mp[sx] *.snm *.nav *.out
	rm -f $(GENERATED)

%.mps: %.1
	mv $^ $@

%.1: %.mp
	mpost $^ end
