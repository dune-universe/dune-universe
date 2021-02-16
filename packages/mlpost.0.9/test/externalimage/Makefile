all : latex beamer

latex: powered-by-caml.png images externalimage.tex
	pdflatex externalimage.tex

beamer: powered-by-caml.png images externalimage_beamer.tex
	pdflatex externalimage_beamer.tex

images : externalimage.ml
	../../tool.opt -ccopt "-I ../../" -pdf externalimage.ml

clean:
	rm -f *.pdf *.aux *.log *.mp *.mps *.1 *.nav *.out *.toc *.snm
