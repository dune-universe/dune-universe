manual: testmanual.pdf

testmanual.pdf: testmanual.tex
	#mpost testmanual.mp end
	make -C manual mps
	pdflatex testmanual.tex
	#dvips testmanual.dvi -o testmanual.ps

other: othergraphs.tex
	#mpost othergraphs.mp end
	make -C othergraphs mps
	pdflatex othergraphs.tex
	#dvips othergraphs.dvi -o othergraphs.ps

tests: tests.dvi
	dvips tests.dvi -o tests.ps

testbox: testbox.dvi
	dvips testbox.dvi -o testbox.ps

%.dvi: %.mp
	mpost $*.mp end
	latex $*

tests.pdf: tests.dvi
	dvipdf tests.dvi


clean: 
	make -C manual clean
	make -C othergraphs clean
	rm -f testmanual.*
	rm -f testmanual_cairo-*.*
	rm -f tests.* *.mps *.pdf
	rm -f othergraphs.*
