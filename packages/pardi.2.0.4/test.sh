#!/bin/bash

set -x
set -u

# rm -f data/decoys_std.smi
# time (standardiser -i data/decoys.smi -o data/decoys_std.smi 2>&1) > /dev/null
# wc -l data/decoys_std.smi
printf "real\t0m6.363s\n"

rm -f data/decoys_std_pardi.smi
time ./pardi -v -c 100 -i data/decoys.smi -ie .smi -oe .smi -o data/decoys_std_pardi.smi \
     -w '(standardiser -i %IN -o %OUT 2>&1) > /dev/null'
wc -l data/decoys_std_pardi.smi

rm -f data/test_out.types
./pardi -n 1 -i data/test_in.types -o data/test_out.types \
        -d 'r:^#atoms:' -w 'cat %IN > %OUT'
diff data/test_in.types data/test_out.types
./pardi -c 6 -i data/test_in.types -o data/test_out.types \
        -d 'r:^#atoms:' -w 'cat %IN > %OUT'
diff data/test_in.types data/test_out.types

rm -f data/still_decoys.mol2
./pardi -i data/decoys.mol2 -o data/still_decoys.mol2 \
        -d 's:@<TRIPOS>MOLECULE' -w 'cp %IN %OUT'
grep -c '@<TRIPOS>MOLECULE' data/decoys.mol2 data/still_decoys.mol2

# test sorted cat mux mode
rm -f data/decoys2.mol2
./pardi -i data/decoys.mol2 -o data/decoys2.mol2 \
        -d b:10000 -w 'cp %IN %OUT' -m s
diff -q data/decoys.mol2 data/decoys2.mol2
