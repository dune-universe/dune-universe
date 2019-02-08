#!/bin/bash

#set -x # debug

make # we need an up to date ./consent executable

# database=mini_nrlist
# max=100
database=VU_QSAR_datasets
max=20
#database=mini_VU_QSAR_datasets
echo=1
clusters=TBD

case "$database" in
    mini_nrlist) \rm -f data/*/*/*.eps data/*/*/*.txt # clean previous run
                 clusters=data/name2cluster.csv
                 ;;
    *VU_QSAR_datasets)
        \rm -f ~/usr/VU_QSAR_datasets/*/*.eps ~/usr/VU_QSAR_datasets/*/*.txt # clean previous run
        clusters=~/usr/VU_QSAR_datasets/attic/name2cluster.csv
        ;;
esac

for fp_type in `echo maccs ecfp4 mop2d`; do
    # run experiments
    for t in `cat $database`; do
        for i in `echo 2 5 10`; do # iter over cons. sizes
            \rm -f results.${fp_type}.${i}.txt
            q=""
            db=""
            case "$fp_type" in
                ecfp4) q=$t"/queries.ecfp4"
                       db=$t"/db.ecfp4";;
                maccs) q=$t"/queries.csv"
                       db=$t"/db.csv";;
                mop2d) q=$t"/queries.mop2d"
                       db=$t"/db.mop2d";;
                *) echo "unsupported fp_type: "$fp_type
                   exit 1;;
            esac
            cmd='./consent -np 8 -v -c '$clusters' -scan -n '$i' -max '$max' -q '$q' -db '$db
            echo $cmd
            if [ "$echo" == "0" ]; then
                echo xp: $t $i ${fp_type}
                ($cmd 2>&1) > ${t}/curr.${fp_type}.${i}.txt
                if [ "$?" != "0" ] ; then
                    echo "commands.sh: FATAL: ./consent crashed"
                    exit 1
                fi
            fi
        done
    done
done
