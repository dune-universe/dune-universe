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
	@rm -rf tmp_boot

.PHONY: tests
tests:
	@dune runtest

.PHONY: install
install: all
	@dune install

.PHONY: uninstall
uninstall: all
	@dune uninstall

VERSION = $(shell ocamlc -version | sed s/+.*//)

.PHONY: boot
boot: all
	@echo "Boot for $(VERSION)"
	@# Reinitialize the [tmp_boot] directory.
	@rm -rf tmp_boot
	@mkdir tmp_boot
	@# Preprocess the [pa_*] files.
	@_build/install/default/bin/pa_ocaml --ascii pa_ocaml/pa_ast.ml \
		> tmp_boot/pa_ast.ml
	@_build/install/default/bin/pa_ocaml --ascii pa_ocaml/pa_default.ml \
		> tmp_boot/pa_default.ml
	@_build/install/default/bin/pa_ocaml --ascii pa_ocaml/pa_lexing.ml \
		> tmp_boot/pa_lexing.ml
	@_build/install/default/bin/pa_ocaml --ascii pa_ocaml/pa_main.ml \
		> tmp_boot/pa_main.ml
	@_build/install/default/bin/pa_ocaml --ascii pa_ocaml/pa_ocaml.ml \
		> tmp_boot/pa_ocaml.ml
	@_build/install/default/bin/pa_ocaml --ascii pa_ocaml/pa_ocaml_prelude.ml \
		> tmp_boot/pa_ocaml_prelude.ml
	@_build/install/default/bin/pa_ocaml --ascii pa_ocaml/pa_parser.ml \
		> tmp_boot/pa_parser.ml
	@# Erase [open Earley_ocaml] from [tmp_boot/pa_default.ml]
	@sed -i 's/open Earley_ocaml//g' tmp_boot/pa_default.ml
	@# Copy the [static/helpers] files.
	@cp static/helpers/helper.mli tmp_boot/helper.mli
	@cp static/helpers/$(VERSION)/* tmp_boot/
	@# Generate [compare.ml].
	@_build/install/default/bin/pa_ocaml --ascii static/tools/generic_eq.ml \
		> tmp_boot/compare.ml
	@echo "(* asttypes.mli *)" \
		>> tmp_boot/compare.ml
	@_build/default/tools/pa_eq.exe static/tools/$(VERSION)/asttypes.mli \
		>> tmp_boot/compare.ml
	@echo "(* parsetree.mli *)" \
		>> tmp_boot/compare.ml
	@_build/default/tools/pa_eq.exe static/tools/$(VERSION)/parsetree.mli \
		>> tmp_boot/compare.ml
	@# Generate [iter.ml].
	@_build/install/default/bin/pa_ocaml --ascii static/tools/generic_iter.ml \
		> tmp_boot/iter.ml
	@echo "(* asttypes.mli *)" \
		>> tmp_boot/iter.ml
	@_build/default/tools/pa_iter.exe static/tools/$(VERSION)/asttypes.mli \
		>> tmp_boot/iter.ml
	@echo "(* parsetree.mli *)" \
		>> tmp_boot/iter.ml
	@_build/default/tools/pa_iter.exe static/tools/$(VERSION)/parsetree.mli \
		>> tmp_boot/iter.ml
	@# Generate [quote.ml].
	@_build/install/default/bin/pa_ocaml --ascii static/tools/generic_quote.ml \
		> tmp_boot/quote.ml
	@echo "(* asttypes.mli *)" \
		>> tmp_boot/quote.ml
	@_build/default/tools/pa_quote.exe static/tools/$(VERSION)/asttypes.mli \
		>> tmp_boot/quote.ml
	@echo "(* parsetree.mli *)" \
		>> tmp_boot/quote.ml
	@_build/default/tools/pa_quote.exe static/tools/$(VERSION)/parsetree.mli \
		>> tmp_boot/quote.ml
	@# Backup and replace boot directory.
	@tar -cf boot_$(VERSION)_$(shell date +%F_%H-%M-%S).tar static/boot/$(VERSION)
	@echo "Backup written to boot_$(VERSION)_$(shell date +%F_%H-%M-%S).tar"
	@rm -rf static/boot/$(VERSION)
	@mv tmp_boot static/boot/$(VERSION)
