# Run tests

TSTS	:= $(patsubst %.gl,%.txt,$(wildcard *.gl)) \
		$(patsubst %.glx,%.txt,$(wildcard *.glx))
CHASE	= ../chase.native
CHASEFLAGS = -t
DIFF	= diff
DIFFFLAGS = -u

%.txt:	%.gl $(CHASE)
	$(CHASE) $(CHASEFLAGS) -o $@ $*.gl
	-$(DIFF) $(DIFFFLAGS) $*.tst $*.txt

%.txt:	%.glx $(CHASE)
	-$(CHASE) $(CHASEFLAGS) -o $@ $*.glx
	-$(DIFF) $(DIFFFLAGS) $*.tst $*.txt

all:	$(TSTS)

clean:
	-rm $(TSTS)
