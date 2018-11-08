#!/bin/bash

DB=mini_nrlist
repeats=50
# DB=VU_QSAR_datasets
# repeats=10

let 'i=0'
for csize in `echo 5 10 20`; do
    for t in `cat $DB`; do
        echo $t
        echo ./cpu_bound_query -q $t/queries.ecfp4 -db $t/db.ecfp4 -n $csize -r $repeats \> $t/cpu_bound_${i}_${csize}.plot
        target=`echo $t | sed 's/\.\/data\///g' | sed 's/\/home\/berengfc\/hdd\/usr\/VU_QSAR_datasets\///g'` 
        gnuplot -persist <<EOF
set xtic out nomirror
set ytics out nomirror
set key bottom right
set xlabel 'Molecule rank'
set ylabel 'Cumulated number of actives'
set title 'target: $target csize: $csize'
plot '$t/cpu_bound_${i}_${csize}.plot' u 2 w l t 'opti', \
                                    '' u 1 w l t 'oppo'
set term postscript eps enhanced color
set output '$t/cpu_bound_${i}_${csize}.eps'
replot
EOF
        let "++i"
    done
done
