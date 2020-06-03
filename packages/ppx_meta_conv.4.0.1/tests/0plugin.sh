#!/bin/sh

export OPAMLIB=`opam config var lib`
ocamlc -verbose -dsource -I $OPAMLIB/ppx_deriving/ -ppx '$OPAMLIB/ppx_deriving/ppx_deriving $OPAMLIB/ppxx/ppxx.cmxs $OPAMLIB/re/re.cmxs $OPAMLIB/re/re_perl.cmxs $OPAMLIB/re/re_pcre.cmxs $OPAMLIB/ppx_test/ppx_test.cmxs $OPAMLIB/ocaml/unix.cmxs $OPAMLIB/ocaml/str.cmxs $OPAMLIB/spotlib/spotlib.cmxs ../src/ppx_deriving_meta_conv.cma' conv_test.ml 

