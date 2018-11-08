#!/bin/bash

# evaluate the effect of cons. size on AUC and Power Metric @ 1%

for n in `echo 1 2 3 4 5 6 7 8 9 10 12 15 20`; do
    ./cons_size_effect -q ~/hdd/usr/VU_QSAR_datasets/463087/queries.ecfp4 \
                      -db ~/hdd/usr/VU_QSAR_datasets/463087/db.ecfp4 -n $n -r 100 2>&1 | grep 'AUC:'
done
