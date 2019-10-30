# Makefile rules for the chase

CHASE     = chase
CHASETREE = chasetree
M4	  = m4

# Run chase in terse mode
%.txt:		%.gl
	$(TIME) $(CHASE) -t $(CHASEFLAGS) -o $@ $*.gl

# Run chase in terse mode expecting errors
%.txt:		%.glx
	-$(TIME) $(CHASE) -t $(CHASEFLAGS) -o $@ $*.glx

# Run chase in verbose mode
%.text:		%.gl
	$(TIME) $(CHASE) $(CHASEFLAGS) -o $@ $*.gl

# Run chase in verbose mode expecting errors
%.text:		%.glx
	-$(TIME) $(CHASE) $(CHASEFLAGS) -o $@ $*.glx

# Use m4 to preprocess
# Include file "foo.gl" by writing
#
# m4_include(`foo.gl')m4_dnl
#
# in a source file with extension .glm
%.gl:		%.glm
	$(M4) -P $(M4FLAGS) $*.glm > $@

# Produce a graphic view of verbose chase output
%.xhtml:	%.text
	$(CHASETREE) $(CHASETREEFLAGS) -o $@ $*.text

.PRECIOUS:	%.text
