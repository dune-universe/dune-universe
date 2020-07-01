#!/bin/bash

set -x

# # find best model architecture using early stopping
# for l in 64 128 256 512 1024 2048; do
#     # hidden layers: 1 to 4
#     echo odnnr_model -b 10 --no-plot --train data/chembl1868_good.pFP \
#          --NxCV 5 --epochs 100 --early-stop --arch $l
#     echo odnnr_model -b 10 --no-plot --train data/chembl1868_good.pFP \
#          --NxCV 5 --epochs 100 --early-stop --arch $l/$l
#     echo odnnr_model -b 10 --no-plot --train data/chembl1868_good.pFP \
#          --NxCV 5 --epochs 100 --early-stop --arch $l/$l/$l
#     echo odnnr_model -b 10 --no-plot --train data/chembl1868_good.pFP \
#          --NxCV 5 --epochs 100 --early-stop --arch $l/$l/$l/$l
# done

# iterated random experiment
for i in `seq 1 50`; do
    # oplsr_model --no-plot --ncomp 11 -p 0.8 --train data/chembl1868_good.pFP
    ./model -b 10 --no-plot --train data/chembl1868_good.pFP \
            --epochs 50 --arch 512/512/512/512/512
done 2>&1 | grep testR2

for i in `seq 1 50`; do
    pqsar_csvrfr --train data/chembl1868_good.pFP --max-feat 542
done 2>&1 | grep R2=
