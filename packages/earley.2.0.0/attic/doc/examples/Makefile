EARLEY_DIR = ../..
PA_OCAML = $(EARLEY_DIR)/pa_ocaml
OPTS=-pp $(PA_OCAML) -linkpkg -package earley -package earley.str -I $(EARLEY_DIR)

all: calc_base calc_prio aabb aabbcc text pa_do_try

calc_base: calc_base.ml $(PA_OCAML)
	ocamlfind ocamlc $(OPTS) -o $@  $<

calc_prio: calc_prio.ml $(PA_OCAML)
	ocamlfind ocamlc $(OPTS) -o $@  $<

calc_prio2: calc_prio2.ml $(PA_OCAML)
	ocamlfind ocamlc $(OPTS) -o $@  $<

aabb: aabb.ml $(PA_OCAML)
	ocamlfind ocamlc $(OPTS) -o $@  $<

aabbcc: aabbcc.ml $(PA_OCAML)
	ocamlfind ocamlc $(OPTS) -o $@  $<

text: text.ml $(PA_OCAML)
	ocamlfind ocamlc $(OPTS) -o $@  $<

pa_do_try: pa_do_try.ml $(PA_OCAML) $(EARLEY_DIR)/earley_ocaml.cmxa
	ocamlfind ocamlopt $(OPTS) -I +compiler-libs -package unix -package earley_ocaml -o $@ $<

clean:
	@rm -f *.cmi *.cmo *.cmx *.o

distclean: clean
	@rm -f calc_base calc_prio aabb aabbcc text pa_do_try
