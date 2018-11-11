#!/bin/bash

TIME=0
COMPARE=0
EXTENSION=0
COLOR=1

ocaml=`ocamlc -where`
local=./tests_pa_ocaml
examples=./doc
diff=./tests_pa_ocaml
ocamlversion=`ocamlc -version`

while [[ $# -gt 0 ]]; do
    key="$1"

    case $key in
        -t|--time)
            TIME=1
            shift
            ;;
        -c|--compare)
            COMPARE=1
            shift
            ;;
        -e|--extension)
            EXTENSION=1
            shift
            ;;
        -n|--no-color)
            COLOR=0
            shift
            ;;
	--clean)
	    rm -f $local/*.ml.* $local/*.mli.*
	    rm -f $local/*/result
	    exit 0
	    ;;
        --all)
            TIME=1
            COMPARE=1
            EXTENSION=1
            shift
            ;;
    esac
done

if [ ${TIME} -eq 0 -a ${COMPARE} -eq 0 -a ${EXTENSION} -eq 0 ]; then
    COMPARE=1
fi

files="$local/long.ml $local/long_mod.ml $local/vlong_mod.ml $local/bibi.ml $local/UTF16.ml $local/test_offset.ml $local/image*.ml $local/decap.ml \
       $local/pa_byt_main.ml $local/pa_ocaml.ml $local/pa_ocaml_prelude.ml $local/pa_parser.ml \
       $local/test.ml $local/objects.ml $local/variants.ml $local/prefix.ml\
       $local/mixin2.ml $local/mixev.ml $local/mixev2.ml $local/mixmod.ml $local/mixmod5.ml $local/mixobj.ml \
       $ocaml/pervasives.ml $ocaml/pervasives.mli $ocaml/list.ml $ocaml/list.mli $ocaml/stack.ml $ocaml/stack.mli\
       $ocaml/set.ml $ocaml/set.mli $ocaml/map.ml $ocaml/map.mli $local/bigarray.ml $ocaml/bigarray.mli \
       $ocaml/string.ml $ocaml/string.mli $ocaml/array.ml $ocaml/array.mli $ocaml/char.ml $ocaml/char.mli \
       $ocaml/arg.ml $ocaml/arg.mli $ocaml/arrayLabels.ml $ocaml/arrayLabels.mli $ocaml/buffer.ml \
       $ocaml/buffer.mli $ocaml/complex.ml $ocaml/complex.mli $ocaml/digest.ml $ocaml/digest.mli \
       $ocaml/dynlink.mli $ocaml/filename.ml $ocaml/filename.mli $ocaml/format.ml $ocaml/gc.ml \
       $ocaml/gc.mli $ocaml/genlex.ml $ocaml/genlex.mli $ocaml/hashtbl.ml $ocaml/hashtbl.mli \
       $ocaml/lexing.ml $ocaml/lexing.mli $ocaml/listLabels.ml $ocaml/listLabels.mli $ocaml/moreLabels.ml \
       $ocaml/moreLabels.mli $local/test4.ml
"

# echo $files

if [ ${TIME} -eq 1 ]; then
    make $MAKEOPTS test_parsers.native
    ./test_parsers.native $files
fi

if [ ${COLOR} -eq 1 ]; then
    red="\e[31m"
    yellow="\e[93m"
    closing="\e[0m"
else
    res=""
    yellow=""
    closing=""
fi

if [ ${COMPARE} -eq 1 ]; then
    for f in $files; do
	n=$(basename $f)
        echo -e "$n: "
        ./pa_ocaml $f > /dev/null

        ocamlc -rectypes -c -dparsetree -o /tmp/foo.cmo -pp ./pa_ocaml  $f 2> $diff/$n.pa_ocaml.full
        ocamlc -rectypes -c -dparsetree -o /tmp/bar.cmo                 $f 2> $diff/$n.ocamlc.full

        cat $diff/$n.pa_ocaml.full | sed -e 's/(.*\.mli\?\[.*\]\.\.\([^[]*\.mli\?\)\?\[.*\])\( ghost\)\?//' > $diff/$n.pa_ocaml
        cat $diff/$n.ocamlc.full | sed -e 's/(.*\.mli\?\[.*\]\.\.\([^[]*\.mli\?\)\?\[.*\])\( ghost\)\?//' > $diff/$n.ocamlc
        diff $diff/$n.pa_ocaml  $diff/$n.ocamlc > $diff/$n.diff
        diff $diff/$n.pa_ocaml.full $diff/$n.ocamlc.full > $diff/$n.fulldiff
        if [ -s $diff/$n.diff ]; then
            echo -e $red   diff size: $(wc -lw $diff/$n.diff) $closing
        fi
        if [ -s $diff/$n.fulldiff ]; then
            echo -e $yellow   diff size with pos: $(wc -lw $diff/$n.fulldiff) $closing
        fi
    done

    echo "********************************************"
    echo TOTAL diff size:
    wc -lw $diff/*.diff | grep total
    echo TOTAL diff size with pos:
    wc -lw $diff/*.fulldiff | grep total
    echo "********************************************"
fi

if [ ${EXTENSION} -eq 1 ]; then
    echo "test of the extensions to the syntax"
    /usr/bin/time --format="%C: %e" ocamlc -c -pp ./pa_ocaml $local/test_ext.ml
    /usr/bin/time --format="%C: %e" ocamlc -i -c -pp ./pa_ocaml -I +compiler-libs -I bootstrap/$ocamlversion $local/test_quotation.ml
    make $examples/pa_do_try
    /usr/bin/time --format="%C: %e" ocamlc -i -c -pp $examples/pa_do_try $local/test_extension.ml
    echo

    # echo "test of parser extension"
    # /usr/bin/time --format="%C: %e" ocamlc -c -I .. -pp ./pa_ocaml ./examples/calc.ml
    # /usr/bin/time --format="%C: %e" ocamlc -c -I .. -pp ./pa_ocaml ./examples/calc_all.ml
    # cp ./pa_ocaml_prelude.ml $local/
    # /usr/bin/time --format="%C: %e" ocamlc -I +compiler-libs -c -I bootstrap/$ocamlversion -pp ./pa_ocaml $local/pa_ocaml_prelude.ml
    # cp ./pa_parser.ml $local/
    # /usr/bin/time --format="%C: %e" ocamlc -I +compiler-libs -c -I bootstrap/$ocamlversion -pp ./pa_ocaml $local/pa_parser.ml
    # cp ./pa_ocaml.ml $local/
    # /usr/bin/time --format="%C: %e" ocamlc -I +compiler-libs -c -I bootstrap/$ocamlversion -pp ./pa_ocaml $local/pa_ocaml.ml
fi
