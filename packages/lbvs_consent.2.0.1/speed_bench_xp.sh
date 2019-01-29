#!/bin/bash

# benchmark time between oppo and opti cons. strat as a function of the cons. size and DB format

t=/home/berengfc/hdd/usr/VU_QSAR_datasets/485290

for csize in `echo 2 3 5 10 20`; do
    for fmt in `echo ecfp4 ecfp4.bin`; do
        for strat in `echo oppo opti`; do
            printf "csize: %d fmt: %s strat: %s\n" $csize $fmt $strat
            xp="./consent -s "$strat" -q "$t"/queries.ecfp4 -db "$t"/db."$fmt" -n "$csize" -top 1000 -o scores_1k.out"
            $xp
            for i in `seq 1 5`; do
                \time -p $xp
            done
        done
    done
done
