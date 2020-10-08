PARSER_INCLUDES += -I $(CAMLP5LIB)
# OCaml parser call with flags
#AM_PXFLAGS = -pp "$(CAMLP4O) $(PARSER_INCLUDES) pa_log.cmo $(LOG)"
PXFLAGS = -pp "$(CAMLP5R) $(PARSER_INCLUDES) pr_o.cmo"

.PHONY: doc
# check if style.css already exists to avoid everytime generation
doc: html/style.css

html/style.css:
	test -d html || mkdir html
	$(OCAMLDOC) $(PARSER_INCLUDES) $(PXFLAGS) -d html -html \
	-I $(top_srcdir)/camlp5 $(top_srcdir)/camlp5/*.ml

# default clean
clean-am:
	rm -fR html/*.html html/*.css
