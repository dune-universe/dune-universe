MAIN=apt-external-solvers

all: html pdf

html: $(MAIN).html
pdf: $(MAIN).pdf

$(MAIN).html:
	pandoc $(MAIN).md -o $(MAIN).html

$(MAIN).pdf:
	pandoc $(MAIN).md -o $(MAIN).pdf

clean:
	-rm -f $(MAIN).html $(MAIN).pdf

.phony: clean all pdf html
