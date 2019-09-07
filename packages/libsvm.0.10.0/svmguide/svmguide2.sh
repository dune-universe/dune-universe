#!/bin/bash

# This script downloads the Bioinformatics dataset mentioned in the SVM guide
# www.csie.ntu.edu.tw/~cjlin/papers/guide/guide.pdf and executes the second
# command set on page 10 with the OCaml command line interface in quiet mode.

CATEGORY=multiclass
TRAINFILE="svmguide2"

source 'common.sh'

download $CATEGORY $TRAINFILE && check_svm_cli

TRAINFILE="$DATADIR/$TRAINFILE"

# remove additional spaces after class labels
sed -i -e 's/^\(+[0-9]\) */\1 /g' $TRAINFILE

$SVMCLI scale -l -1 -u 1 $TRAINFILE > $TRAINFILE.scale &&
$SVMCLI train -q -v 5 $TRAINFILE.scale
