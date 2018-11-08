#!/bin/bash

set -x

# DB=VU_QSAR_activity
# nb_repeats=10
DB=mini_nrlist
nb_repeats=500

let "i=0"
for t in `cat $DB` ; do
    for csize in `echo 10 15 20`; do
        echo $t $csize
        ./pot_scale_doctor -r $nb_repeats -n $csize -q $t/queries.ecfp4 -db $t/db.ecfp4 > $t/pot_scale_ecfp4_${csize}.toplot
        gnuplot -persist <<EOF
set title 'csize: $csize target: $t'
set xlabel 'active rank (decreasing potency order)'
set ylabel 'median delta rank over $nb_repeats experiments (real Vs. know policy)'
f(x) = a*x + b
fit f(x) '$t/pot_scale_ecfp4_${csize}.toplot' u 1:2 via a,b
plot f(x) not, '$t/pot_scale_ecfp4_${csize}.toplot' u 1:2 w imp not
set output 'delta_rank_ecfp4_${csize}_$i.eps'
set term postscript eps enhanced color
replot
EOF
    done
    let "++i"
done
