#!/bin/bash

set -x

dune build @all

_build/default/src/test.exe --train data/solubility_train_std_01.csv \
                            --test data/solubility_test_std_01.csv \
                            -n 841 -v
