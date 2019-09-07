#!/bin/bash

URL="http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets"
DATADIR="./data"
SVMCLI="../svm_cli.native"

download ()
{
  CATEGORY=$1
  shift
  mkdir -p $DATADIR
  for file in $*; do
    if [ ! -f "$DATADIR/$file" ]; then
      printf "Downloading file '$file'..."
      if wget -q -P $DATADIR "$URL/$CATEGORY/$file"; then
        echo " done!"
      else
        echo " failed!"; exit 1
      fi
    fi
  done
}

check_svm_cli ()
{
  if [ ! -f "$SVMCLI" ]; then
    echo "$SVMCLI not found. You have to build it first."
    exit 1
  fi
}
