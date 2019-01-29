#!/bin/bash

#set -x

DB=mini_VU_QSAR_datasets
DB=mini_nrlist

for t in `cat $DB` ; do
    ./ic50_doctor -i $t/queries.csv > $t/ic50_weight.toplot
done

gnuplot -persist <<EOF
set xlabel 'IC50'
set ylabel 'weight'
plot './data/ER_alpha/+/ic50_weight.toplot' u 1:2 w l, \
     './data/ER_alpha/-/ic50_weight.toplot' u 1:2 w l, \
     './data/RAR_alpha/-/ic50_weight.toplot' u 1:2 w l, \
     './data/ER_beta/+/ic50_weight.toplot' u 1:2 w l, \
     './data/GR/+/ic50_weight.toplot' u 1:2 w l, \
     './data/GR/-/ic50_weight.toplot' u 1:2 w l, \
     './data/AR/-/ic50_weight.toplot' u 1:2 w l, \
     './data/RXR_alpha/-/ic50_weight.toplot' u 1:2 w l, \
     './data/PR/+/ic50_weight.toplot' u 1:2 w l, \
     './data/PR/-/ic50_weight.toplot' u 1:2 w l
EOF
