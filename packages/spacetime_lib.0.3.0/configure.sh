#! /bin/bash

MAJOR=`ocamlc -config | sed -n 's/^version: *\(.*\)\.\(.*\)\\.\(.*\).*$/\1/p'`
MINOR=`ocamlc -config | sed -n 's/^version: *\(.*\)\.\(.*\)\\.\(.*\).*$/\2/p'`

if [ -h src/versioned.ml ]; then
  rm src/versioned.ml
fi

if [ "$MAJOR" = "4" ]; then
    if [ "$MINOR" -lt "03" ]; then
        echo "OCaml version $MAJOR.$MINOR not supported"
    elif [ "$MINOR" -lt "06" ]; then
        echo "Version 4.04 -- 4.05"
        ln -s ./versioned/4.04/versioned.ml src
    else
        echo "Version 4.06 --"
        ln -s ./versioned/4.06/versioned.ml src
    fi
else
    echo "OCaml version $MAJOR.$MINOR not supported"
fi
