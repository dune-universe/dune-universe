#!/bin/bash

./pardi -c 10 -i data/decoys.mol2 -o data/decoys2.mol2 -d b:1000 \
        -w 'in=%IN; out=%OUT; /bin/cp $in $out; ls -l $in $out' -m s > log

# if you get a 'No such file or directory', the bug just happened
