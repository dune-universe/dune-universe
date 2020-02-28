# Run literate programming examples

include ../chase.mk

TXTS	:= $(patsubst %.md, %.txt, $(filter-out README.md, $(wildcard *.md)))
XHTMLS	= $(patsubst %.txt, %.xhtml, $(TXTS))

all:	$(TXTS) $(XHTMLS)

clean:
	-rm $(TXTS) $(patsubst %.txt, %.text, $(TXTS)) $(XHTMLS)
