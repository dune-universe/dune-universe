#!/bin/bash

# This script downloads the Astroparticle dataset mentioned in the SVM guide
# www.csie.ntu.edu.tw/~cjlin/papers/guide/guide.pdf and executes the second
# command set on page 9 with the OCaml command line interface in quiet mode.

CATEGORY="binary"
TRAINFILE="svmguide1"
TESTFILE="svmguide1.t"

source 'common.sh'

download $CATEGORY $TRAINFILE $TESTFILE && check_svm_cli

TRAINFILE="$DATADIR/$TRAINFILE"
TESTFILE="$DATADIR/$TESTFILE"
SCALEPARAMSFILE="$DATADIR/range1"

$SVMCLI scale -l -1 -u 1 -s $SCALEPARAMSFILE $TRAINFILE > $TRAINFILE.scale &&
$SVMCLI scale -r $SCALEPARAMSFILE $TESTFILE > $TESTFILE.scale &&
$SVMCLI train -q $TRAINFILE.scale &&
$SVMCLI predict $TESTFILE.scale $TRAINFILE.scale.model $TESTFILE.predict
