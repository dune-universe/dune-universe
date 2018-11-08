#!/bin/bash

set -x # verbose run

# erase previous run
rm -f data/pcid_435034_act_dec_std_rand.1mop2d \
      data/pcid_435034_act_dec_std_rand.idx \
      data/pcid_435034_act_dec_std_rand.mol2 \
      data/pcid_435034_act_dec_std_rand.smi \
      data/test.1mop2d \
      data/test.1mop2d.dbbad \
      data/train.1mop2d \
      data/train.1mop2d.dbbad \
      scan.log

# extract test data
xz --keep --decompress data/pcid_435034_act_dec_std_rand.smi.xz
NB_LINES=`wc -l data/pcid_435034_act_dec_std_rand.smi | awk '{print $1}'`
HALF=$(($NB_LINES/2))
HALF_PLUS_ONE=$((1 + $NB_LINES/2))

# encode molecules
obabel data/pcid_435034_act_dec_std_rand.smi \
    -O data/pcid_435034_act_dec_std_rand.mol2

hts_shrink_index -i data/pcid_435034_act_dec_std_rand.mol2 -r 1 \
                 -o data/pcid_435034_act_dec_std_rand.idx

hts_shrink_encode -i data/pcid_435034_act_dec_std_rand.mol2 \
                  -o data/pcid_435034_act_dec_std_rand.1mop2d \
                  -idx data/pcid_435034_act_dec_std_rand.idx

# randomize dataset
head -1 data/pcid_435034_act_dec_std_rand.1mop2d > data/rand.1mop2d
get_line -r 2..$HALF_PLUS_ONE \
         -i data/pcid_435034_act_dec_std_rand.1mop2d --rand >> data/rand.1mop2d

# create training set
head -$HALF_PLUS_ONE data/rand.1mop2d > data/train.1mop2d

# create test set
head -1 data/rand.1mop2d > data/test.1mop2d # header line
tail -$HALF data/rand.1mop2d >> data/test.1mop2d

# run DBBAD
hts_shrink_dbbad --train data/train.1mop2d --test data/test.1mop2d \
                 --dscan scan.log
