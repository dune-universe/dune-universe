#!/bin/bash

set -x # DEBUG

gunzip -f -k data/training.liblinear.gz

make

rm -f model_{01,02,05,10,20,50}.txt

./linwrap -np 16 -q -i data/training.liblinear -s model_01.txt -k 1
./linwrap -np 16 -q -i data/training.liblinear -s model_02.txt -k 2
./linwrap -np 16 -q -i data/training.liblinear -s model_05.txt -k 5
./linwrap -np 16 -q -i data/training.liblinear -s model_10.txt -k 10
./linwrap -np 16 -q -i data/training.liblinear -s model_20.txt -k 20
./linwrap -np 16 -q -i data/training.liblinear -s model_50.txt -k 50

./linwrap -np 16 -i data/training.liblinear -l model_01.txt -o /tmp/test_01.txt \
          2>&1 | grep AUC
./linwrap -np 16 -i data/training.liblinear -l model_02.txt -o /tmp/test_02.txt \
          2>&1 | grep AUC
./linwrap -np 16 -i data/training.liblinear -l model_05.txt -o /tmp/test_05.txt \
          2>&1 | grep AUC
./linwrap -np 16 -i data/training.liblinear -l model_10.txt -o /tmp/test_10.txt \
          2>&1 | grep AUC
./linwrap -np 16 -i data/training.liblinear -l model_20.txt -o /tmp/test_20.txt \
          2>&1 | grep AUC
./linwrap -np 16 -i data/training.liblinear -l model_50.txt -o /tmp/test_50.txt \
          2>&1 | grep AUC
