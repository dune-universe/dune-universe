#!/bin/bash

set -x

NPROCS=`getconf _NPROCESSORS_ONLN`

dune build src/test.exe

time _build/default/src/test.exe -n 1 -np 1 \
     -feats 0.1 -samps 1.0 -min 1 \
     --class -tr data/train.csv -te data/test.csv

time _build/default/src/test.exe -n 5 -np $NPROCS \
     -feats 0.1 -samps 1.0 -min 1 \
     --class -tr data/train.csv -te data/test.csv

time _build/default/src/test.exe -n 5 -np 1 \
     -feats 0.1 -samps 1.0 -min 1 \
     --regr -rtr data/train_regr.csv -rte data/test_regr.csv

time _build/default/src/test.exe -n 5 -np $NPROCS \
     -feats 0.1 -samps 1.0 -min 1 \
     --regr -rtr data/train_regr.csv -rte data/test_regr.csv

time _build/default/src/test.exe -n 100 -np $NPROCS \
     -feats 1.0 -samps 1.0 -min 1 \
     --regr -rtr data/train_regr.csv -rte data/test_regr.csv
