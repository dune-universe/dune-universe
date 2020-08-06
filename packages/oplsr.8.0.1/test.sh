#!/bin/bash

set -x # DEBUG

make

_build/default/src/model.exe -v \
  --train data/solubility_train_std_01.csv \
  --test data/solubility_test_std_01.csv
