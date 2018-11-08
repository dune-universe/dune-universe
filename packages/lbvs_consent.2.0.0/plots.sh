#!/bin/bash

#set -x # debug

#database=mini_nrlist # all targets with >= 80 ligands
database=VU_QSAR_datasets
#database=mini_VU_QSAR_datasets
data_dir=TBD

case "$database" in
    mini_nrlist) data_dir=data
                 ;;
    *VU_QSAR_datasets) data_dir=~/usr/VU_QSAR_datasets
                      ;;
esac

# do plots
for fp_type in `echo maccs ecfp4 mop2d`; do
# AUC line format
# 1          2            3    4 5     6    7    8 9   10
# 2017-01-10 11:03:01.848 INFO : strat know size 2 AUC 0.784

    # prepare CDFs for gnuplot
    \rm -f *.toplot
    for cons in `echo opti real sing oppo`; do
        # for i in `echo 002 003 005 010 020`; do # iter over cons. sizes
        for i in `echo 002 005 010`; do # iter over cons. sizes
            echo CDF curve $cons $i
            find $data_dir -name auc.${cons}_${fp_type}.${i}.txt -exec cat {} \; > $cons.$i.toplot
            find $data_dir -name pm.${cons}_${fp_type}.${i}.txt -exec cat {} \; > pm.$cons.$i.toplot
            nb_lines=`cat $cons.$i.toplot | wc -l`
            if [ "$nb_lines" == "0" ] ; then
                echo "commands.sh: FATAL: empty "$cons.$i.toplot
                exit 1
            fi
            # compute CDFs
            \rm -f $cons.$i.CDF pm.$cons.$i.CDF
            for x in `seq 0.0 0.05 1.0`; do
                curr=`awk -v x=$x '($2 <= x){print $0}' $cons.$i.toplot | wc -l`
                p=`echo $curr/$nb_lines | bc -l`
                echo $x $p >> $cons.$i.CDF
                curr=`awk -v x=$x '($2 <= x){print $0}' pm.$cons.$i.toplot | wc -l`
                p=`echo $curr/$nb_lines | bc -l`
                echo $x $p >> pm.$cons.$i.CDF
            done
            \cp -f $cons.$i.CDF $cons.$fp_type.$i.CDF
            \cp -f pm.$cons.$i.CDF pm.$cons.$fp_type.$i.CDF
        done
    done
    \rm -f {2,3,5,10,20}.CDF.eps pm.{2,3,5,10,20}.CDF.eps
    gnuplot CDF.gpl
    mv -f 2.CDF.eps   2.CDF.${fp_type}.eps
    mv -f 3.CDF.eps   3.CDF.${fp_type}.eps
    mv -f 5.CDF.eps   5.CDF.${fp_type}.eps
    mv -f 10.CDF.eps 10.CDF.${fp_type}.eps
    mv -f 20.CDF.eps 20.CDF.${fp_type}.eps
    mv -f pm.2.CDF.eps   pm.2.CDF.${fp_type}.eps
    mv -f pm.3.CDF.eps   pm.3.CDF.${fp_type}.eps
    mv -f pm.5.CDF.eps   pm.5.CDF.${fp_type}.eps
    mv -f pm.10.CDF.eps pm.10.CDF.${fp_type}.eps
    mv -f pm.20.CDF.eps pm.20.CDF.${fp_type}.eps

    # plots of accumulated number of actives
    for t in `cat $database`; do
        for i in `echo 002 003 005 010 020`; do
        # for i in `echo 002`; do
            echo accum actives $t $i
            x_max=`tail -1 $t/cum.oppo_${fp_type}.$i.txt | awk '{print $1}'`
            y_max=`tail -1 $t/cum.oppo_${fp_type}.$i.txt | awk '{print $2}'`
            a=`awk -v xmax=$x_max '($1 <= xmax/10){print $2}' $t/cum.sing_${fp_type}.$i.txt | tail -1`
            b=`awk -v xmax=$x_max '($1 <= xmax/10){print $2}' $t/cum.oppo_${fp_type}.$i.txt | tail -1`
            c=`awk -v xmax=$x_max '($1 <= xmax/10){print $2}' $t/cum.opti_${fp_type}.$i.txt | tail -1`
            d=`awk -v xmax=$x_max '($1 <= xmax/10){print $2}' $t/cum.real_${fp_type}.$i.txt | tail -1`
            y_max_zoom=`printf "%f\n%f\n%f\n%f\n%f\n" $a $b $c $d | sort -n | tail -1`
            gnuplot <<EOF
        # cat<<EOF
set size square
set key bottom right
set xtics out nomirror
set ytics out nomirror
set xlabel 'molecule rank'
set ylabel 'accumulated number of actives'
set xrange [0:$x_max]
set yrange [0:$y_max]
set term postscript eps enhanced color
set output '${t}/cum_${fp_type}.${i}.eps'
f(x) = x*(${y_max}/${x_max}) # random efficacy curve
# FBR: for this one we can plot the optimum
plot f(x) not ls -1 , \
     '${t}/cum.oppo_${fp_type}.${i}.txt' u 1:2 w l t 'oppo' lt 1 lc 2, \
     '${t}/cum.opti_${fp_type}.${i}.txt' u 1:2 w l t 'opti' lt 1 lc 3, \
     '${t}/cum.real_${fp_type}.${i}.txt' u 1:2 w l t 'real' lt 1 lc 5, \
     '${t}/cum.sing_${fp_type}.${i}.txt' u 1:2 w l t 'sing' lt 1 lc 8
# zoomed version
set key top left
set output '${t}/cum_${fp_type}_zoomed.${i}.eps'
unset size
set xrange [0:$x_max/10]
set xlabel 'molecule rank (max=10%(DB))'
set yrange [0:$y_max_zoom]
replot
EOF
        done
    done

    # plots of accumulated activity curves
    for t in `cat $database`; do
        for i in `echo 002 003 005 010 020`; do
        # for i in `echo 002`; do
            echo accum activity $t $i
            x_max=`tail -1 $t/cum_act.oppo_${fp_type}.$i.txt | awk '{print $1}'`
            y_max=`tail -1 $t/cum_act.oppo_${fp_type}.$i.txt | awk '{print $2}'`
            a=`awk -v xmax=$x_max '($1 <= xmax/10){print $2}' $t/cum_act.sing_${fp_type}.$i.txt | tail -1`
            b=`awk -v xmax=$x_max '($1 <= xmax/10){print $2}' $t/cum_act.oppo_${fp_type}.$i.txt | tail -1`
            c=`awk -v xmax=$x_max '($1 <= xmax/10){print $2}' $t/cum_act.opti_${fp_type}.$i.txt | tail -1`
            d=`awk -v xmax=$x_max '($1 <= xmax/10){print $2}' $t/cum_act.real_${fp_type}.$i.txt | tail -1`
            y_max_zoom=`printf "%f\n%f\n%f\n%f\n%f\n" $a $b $c $d | sort -n | tail -1`
            gnuplot <<EOF
        # cat<<EOF
set size square
set key bottom right
set xtics out nomirror
set ytics out nomirror
set xlabel 'molecule rank'
set ylabel 'accumulated activity (log(IC50))'
set xrange [0:$x_max]
set yrange [0:$y_max]
set term postscript eps enhanced color
set output '${t}/cum_act_${fp_type}.${i}.eps'
f(x) = x*(${y_max}/${x_max}) # random efficacy curve
plot f(x) not ls -1 , \
     '${t}/cum_act.oppo_${fp_type}.${i}.txt'  u 1:2 w l t 'oppo' lt 1 lc 2, \
     '${t}/cum_act.opti_${fp_type}.${i}.txt'  u 1:2 w l t 'opti' lt 1 lc 3, \
     '${t}/cum_act.real_${fp_type}.${i}.txt'  u 1:2 w l t 'real' lt 1 lc 5, \
     '${t}/cum_act.sing_${fp_type}.${i}.txt'  u 1:2 w l t 'sing' lt 1 lc 8
# zoomed version
set key top left
set output '${t}/cum_act_${fp_type}_zoomed.${i}.eps'
unset size
set xrange [0:$x_max/10]
set xlabel 'molecule rank (max=10%(DB))'
set yrange [0:$y_max_zoom]
replot
EOF
        done
    done

    # plots of cumulated chemical diversity curves
    for t in `cat $database`; do
        for i in `echo 002 003 005 010 020`; do
        # for i in `echo 002`; do
            echo accum chemdiv $t $i
            x_max=`tail -1 $t/chem_div.oppo_${fp_type}.$i.txt | awk '{print $1}'`
            y_max=`tail -1 $t/chem_div.oppo_${fp_type}.$i.txt | awk '{print $2}'`
            a=`awk -v xmax=$x_max '($1 <= xmax/10){print $2}' $t/chem_div.sing_${fp_type}.$i.txt | tail -1`
            b=`awk -v xmax=$x_max '($1 <= xmax/10){print $2}' $t/chem_div.oppo_${fp_type}.$i.txt | tail -1`
            c=`awk -v xmax=$x_max '($1 <= xmax/10){print $2}' $t/chem_div.opti_${fp_type}.$i.txt | tail -1`
            d=`awk -v xmax=$x_max '($1 <= xmax/10){print $2}' $t/chem_div.real_${fp_type}.$i.txt | tail -1`
            y_max_zoom=`printf "%f\n%f\n%f\n%f\n%f\n" $a $b $c $d | sort -n | tail -1`
            n=`echo $y_max + 1 | bc`
            # create optimal curve
            opt=${t}/opt_chem_div.oppo_${fp_type}.${i}.txt
            \rm -f $opt
            for j in `seq 1 $n`; do
                let "k=j-1"
                printf "%d %d\n" $k $j >> ${opt}
            done
            gnuplot <<EOF
        # cat<<EOF
set size square
set key bottom right
set xtics out nomirror
set ytics out nomirror
set xlabel 'molecule rank'
set ylabel 'accumulated chemical diversity'
set xrange [0:$x_max]
set yrange [0:$y_max]
f(x) = x*(${y_max}/${x_max}) # random efficacy curve
set term postscript eps enhanced color
set output '${t}/chem_div_${fp_type}.${i}.eps'
plot f(x) not ls -1 , \
     '${t}/opt_chem_div.oppo_${fp_type}.${i}.txt' u 1:2 w l not ls -1, \
     '${t}/chem_div.oppo_${fp_type}.${i}.txt' u 1:2 w l t 'oppo' lt 1 lc 2, \
     '${t}/chem_div.opti_${fp_type}.${i}.txt' u 1:2 w l t 'opti' lt 1 lc 3, \
     '${t}/chem_div.real_${fp_type}.${i}.txt' u 1:2 w l t 'real' lt 1 lc 5, \
     '${t}/chem_div.sing_${fp_type}.${i}.txt' u 1:2 w l t 'sing' lt 1 lc 8
# zoomed version
set key bottom right
set output '${t}/chem_div_${fp_type}_zoomed.${i}.eps'
unset size
set xrange [0:$x_max/10]
set yrange [0:$y_max_zoom]
set xlabel 'molecule rank (max=10%(DB))'
replot
EOF
        done
    done

done

# example normalization with gnuplot
#  ... u (\$1/$x_max):(\$2/$y_max) w l ...

case "$database" in
    mini_nrlist) make plotsNR.pdf
                 ;;
    *VU_QSAR_datasets) make plotsVU.pdf
                      ;;
esac
