#!/bin/env sh
exec dune exec --profile=release src/bin/smtlib_cat.exe -- $@
