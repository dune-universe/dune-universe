.PHONY: note

default: note
	
note:
	latex note && bibtex note && latex note && pdflatex note

clean:
	rm -f *.aux *.log *.pdf
