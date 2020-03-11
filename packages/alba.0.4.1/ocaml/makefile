.PHONY: alba.base                  \
	alba                       \
	test                       \
        test_library               \
        test_draft                 \
        test_example               \
        test_example_book          \
        unit_test                  \
        unit_test.byte             \
        unit_test.d.byte           \
        test2                      \
        test2_library              \
        test2_draft                \
        test2_example              \
        test2_sf                   \
        alba.byte                  \
        alba2.byte                 \
        alba.d.byte                \
        alba.native                \
        alba2.native               \
        alba.p.native              \
        alba_node.byte             \
        alba_web                   \
        alba_web.byte              \
        alba_draft.byte            \
        alba_draft.native          \
        boolean                    \
        predicate                  \
        tuple                      \
        predicate_logic            \
        function_logic             \
        lattice                    \
        partial_order              \
        endorelation

OCB_FLAGS = -use-ocamlfind \
            -Is lib,_draft \
            -yaccflags --explain,--dump,--infer,--fixed-exception \
            -lib unix

OCB = ocamlbuild $(OCB_FLAGS)

OCBJS_FLAGS = -use-ocamlfind \
	      -cflag -g -lflag -g -Is lib,_draft,js \
              -yaccflags --table,--inspection,--explain,--dump,--infer,--fixed-exception \
	      -pkgs menhirLib,js_of_ocaml,js_of_ocaml.ppx

OCBJS = ocamlbuild $(OCBJS_FLAGS)

alba: alba.native


alba.byte:
	$(OCB) alba.byte

alba2.byte:
	$(OCB) alba2.byte

alba.d.byte:
	$(OCB) alba.d.byte

alba.native:
	$(OCB) alba.native

alba2.native:
	$(OCB) alba2.native

alba.p.native:
	$(OCB) alba.p.native

unit_test.byte:
	$(OCB) unit_test.byte

unit_test.d.byte:
	$(OCB) unit_test.d.byte

unit_test: unit_test.byte
	./unit_test.byte

alba_draft.byte:
	$(OCB) alba_draft.byte

alba_draft.native:
	$(OCB) alba_draft.native


alba-node: alba_node.byte
	js_of_ocaml \
	    --custom-header='#!/usr/bin/env node' \
	    --pretty       \
	    --sourcemap    \
            -o alba-node   \
	    alba_node.byte;\
        chmod u+x alba-node

js/alba_web.js: _build/js/alba_web.byte
	js_of_ocaml \
	    --pretty     \
	    --sourcemap  \
            -o js/alba_web.js  \
	    alba_web.byte

alba_web.byte:
	$(OCBJS) alba_web.byte

alba_web: alba_web.byte js/alba_web.js

alba_node.byte:
	ocamlbuild -use-ocamlfind             \
	-pkgs js_of_ocaml,js_of_ocaml.syntax  \
	-syntax camlp4o                       \
	alba_node.byte

alba.base: alba.native
	./alba.native -work-dir ../library/alba.base init;  \
	./alba.native -work-dir ../library/alba.base compile

doc:
	ocamlbuild eiffelc.docdir/index.html

test2: alba.byte test2_library test2_draft test2_example


test2_library: alba.byte
	./alba.byte -work-dir ../library/alba.base compile -force -verbosity 1

test2_draft: alba.byte
	./alba.byte -I ../library -work-dir ../_draft compile -force -verbosity 1

test2_example: alba.byte
	./alba.byte -I ../library -work-dir ../example compile -force -verbosity 1

test2_sf: alba.byte
	./alba.byte -I ../library -work-dir ../../sf compile -force -verbosity 1

test: alba test_library test_draft test_example test_example_book

test_library: alba
	./alba.native -work-dir ../library/alba.base compile -force -verbosity 1

test_draft: alba
	./alba.native -I ../library -work-dir ../_draft compile -force -verbosity 1

test_example: alba
	./alba.native -I ../library -work-dir ../example compile -force -verbosity 1

test_example_book: alba
	./alba.native -I ../library -work-dir ../../examples/src \
        compile -force -verbosity 1

boolean: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 boolean 1>&2

any: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 any 1>&2

boolean_logic: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 boolean_logic 1>&2

predicate: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 predicate 1>&2


tuple: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 tuple 1>&2


predicate_logic: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 predicate_logic 1>&2


function_logic: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 function_logic 1>&2


relation: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 relation 1>&2


lattice: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 lattice 1>&2

semilattice: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 semilattice 1>&2

complete_semilattice: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 complete_semilattice 1>&2

partial_order: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 partial_order 1>&2

endorelation: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 endorelation 1>&2

function: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 function 1>&2

linear_order: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 linear_order 1>&2

wellorder: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 wellorder 1>&2

natural: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 natural 1>&2

list: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 list 1>&2

binary_tree: alba.byte
	./alba.byte -work-dir ../library/alba.base  \
         compile -force -verbosity 3 binary_tree 1>&2

tree: alba.byte
	./alba.byte -work-dir ../_draft  \
         compile -force -verbosity 3 tree 1>&2




# to compile for debugging use '-cflag -g -lflag -g




clean:
	ocamlbuild -clean
