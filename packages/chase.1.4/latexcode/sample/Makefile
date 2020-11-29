TEX_SRCS := $(wildcard *.tex)

%.pdf:	%.tex
	pdflatex $*
	pdflatex $*

%.gl:	%.tex
	latexcode $(LATEXCODEFLAGS) -o $@ $*.tex

%.txt:	%.gl
	chase -t $(CHASEFLAGS) -o $@ $*.gl

all:	$(TEX_SRCS:.tex=.pdf) $(TEX_SRCS:.tex=.txt)

clean:
	-rm $(TEX_SRCS:.tex=.pdf) $(TEX_SRCS:.tex=.log) $(TEX_SRCS:.tex=.aux)
	-rm $(TEX_SRCS:.tex=.txt) $(TEX_SRCS:.tex=.gl)

.PRECIOUS:	%.gl

.PHONY:	all clean
