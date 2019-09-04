#!/bin/bash

set -o errexit

(
    grep "RESET_TYPE(" src/Reset/ResetPervasives.ml | grep -v "'" | sed "s/ *RESET_TYPE(.*, \(.*\)).*/let (_: General.\1 option) = None/" | sed "s/__/./g" | sort -u | grep -v "\.todo option) = None$";
    grep "RESET_TYPE(.*'" src/Reset/ResetPervasives.ml | sed "s/ *RESET_TYPE(.*, \(.*\)).*/let (_: _ General.\1 option) = None/" | sed "s/__/./g" | sort -u | grep -v "\.todo option) = None$";
    grep "RESET_VALUE" src/Reset/ResetPervasives.ml | sed "s/ *RESET_VALUE(.*, \(.*\)).*/let _ = General.\1/" | sed "s/__/./g" | sort -u | grep -v "\.todo$";
    echo "";
    echo "open General.Abbr";
    echo "";
    echo "let () =";
    echo "  let argv = Li.of_array OCamlStandard.Sys.argv in";
    echo "  Exit.exit (Tst.command_line_main ~argv General.Tests.test)";
) > unit_tests.ml


for OCAML_VERSION in ${OCAML_VERSIONS:-4.02 4.03 4.04 4.05 4.06 4.07 4.08}
do
    echo "OCaml $OCAML_VERSION"
    echo "=========="

    echo
    echo "Building docker image"
    echo "---------------------"

    # Uncomment next line if you're too anxious to wait for the quiet build :)
    # docker build --build-arg OCAML_VERSION=$OCAML_VERSION .
    IMAGE=$(docker build --quiet --build-arg OCAML_VERSION=$OCAML_VERSION .)
    RUN="docker run --rm --volume $PWD:/project --workdir /project $IMAGE"

    mkdir -p _builds/$OCAML_VERSION
    rm -f _build
    ln -sf _builds/$OCAML_VERSION _build

    echo
    echo "Running tests"
    echo "-------------"

    # @todo Measure test coverage. If possible, module by module.
    $RUN dune runtest

    echo
    echo "Extracting interface as seen in utop"
    echo "------------------------------------"

    rm -rf doc/utop/$OCAML_VERSION
    mkdir -p doc/utop/$OCAML_VERSION
    $RUN python3 doc/utop/extract.py doc/utop/$OCAML_VERSION

    rm _build

    echo
    echo "Testing package install"
    echo "-----------------------"

    $RUN opam install General

    # @todo Build demo apps (as native, byte code, and js)

    # @todo Integrate validation of ResetPervasives with dune:
    # in a demo app, check that:
    #  - all symbols in OCamlStandard.Pervasives are reset in ResetPervasives
    #  - all symbols in ResetPervasives do exist in OCamlStandard.Pervasives
    # Symbols: modules, types, exceptions, values, externals

    echo
done

echo
echo "Development cycle OK"

# @todo Build doc
# if (which sphinxcontrib-ocaml-autodoc && which sphinx-build) >/dev/null
# then
#     echo
#     echo "Building doc"
#     echo

#     rm -rf _build/sphinx  # Keep while we're developing the Sphinx extension
#     sphinx-build doc _build/sphinx/html -d _build/sphinx/doctrees
#     rm -rf docs
#     cp -r _build/sphinx/html docs
#     touch docs/.nojekyll
#     rm -f docs/.buildinfo
#     echo
#     echo "See documentation in $(pwd)/docs/index.html"
# else
#     echo
#     echo "Not trying to build doc because autoocamldoc or sphinx-build is missing"
# fi
