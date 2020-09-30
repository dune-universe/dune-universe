.PHONY: all
all:
	@dune build

.PHONY: doc
doc:
	@dune build @doc

.PHONY: clean
clean:
	@dune clean

.PHONY: distclean
distclean: clean
	@find . -name "*~" -exec rm {} \;
	@rm -rf new_boot

.PHONY: tests
tests:
	@dune runtest

.PHONY: install
install: all
	@dune install

.PHONY: uninstall
uninstall: all
	@dune uninstall

.PHONY: boot
boot:
	@echo "Bootstrapping [pa_ocaml] (with backup in a tarball)."
	@tar -cf boot_$(shell date +%F_%H-%M-%S).tar boot
	@rm -rf new_boot && mkdir new_boot
	@dune exec -- pa_ocaml pa_ocaml/pa_ast.ml > new_boot/pa_ast.ml
	@dune exec -- pa_ocaml pa_ocaml/pa_default.ml > new_boot/pa_default.ml
	@dune exec -- pa_ocaml pa_ocaml/pa_lexing.ml > new_boot/pa_lexing.ml
	@dune exec -- pa_ocaml pa_ocaml/pa_main.ml > new_boot/pa_main.ml
	@dune exec -- pa_ocaml pa_ocaml/pa_ocaml.ml > new_boot/pa_ocaml.ml
	@dune exec -- pa_ocaml pa_ocaml/pa_ocaml_prelude.ml > new_boot/pa_ocaml_prelude.ml
	@dune exec -- pa_ocaml pa_ocaml/pa_parser.ml > new_boot/pa_parser.ml
	@rm -f boot/*.ml && mv new_boot/*.ml boot/ && rm -rf new_boot
