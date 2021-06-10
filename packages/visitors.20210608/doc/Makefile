export VISITORS_BUILDING_DOCUMENTATION=true

include ../Makefile.preprocess

export TEXINPUTS=.:../test:../test/ppx_import:

SOURCEDIRS := ../test,../test/ppx_import
ML         := $(shell ls {$(SOURCEDIRS)}/*.ml | grep -v cppo | grep -v processed)
PROCESSED  := $(patsubst %.ml,%.processed.ml,$(ML))

.PHONY: all loop clean sources

all: sources
	latexmk -g -pdf manual

loop: sources
	latexmk -pdf -pvc manual

clean:
	rm -f *.log *.aux *.bbl *.blg *.out *.toc *~ manual.pdf

sources: $(wildcard *.tex) $(wildcard *.bib) $(wildcard *.sty) $(ML) $(PROCESSED)
