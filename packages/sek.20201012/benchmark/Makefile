##############################################################################
# Shortcuts for invoking ./make.sh (without a second argument).

.PHONY: all view clean realclean run plot list
all view clean realclean run plot list:
	./make.sh $@

##############################################################################
# Creating the required opam switch.
# Run this only once.
# This takes several minutes.

SWITCH=$(shell ./make.sh switch)

.PHONY: setup
setup:
	@ CURRENT=`opam switch show` ; \
	if opam switch list | grep $(SWITCH) >/dev/null ; then \
	  echo "The switch $(SWITCH) already exists." ; \
	  opam switch $(SWITCH) ; \
	else \
	  echo "Creating switch $(SWITCH)..." ; \
	  opam switch create $(SWITCH) ; \
	fi ; \
	eval $$(opam env) ; \
	echo "Installing required packages..." ; \
	opam install --yes "dune>=2.0" "cppo" "pprint>=20200410" "seq" "core>=v0.13" ; \
	echo "Switching back to $$CURRENT..." ; \
	opam switch set $$CURRENT >/dev/null ; \
