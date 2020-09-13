#!/bin/bash
set -e

COMMIT=$1
NAME=$2

if [ -z $COMMIT ]; then
    echo Using commit=master
    COMMIT=master
fi

if [ -z $NAME ]; then
    echo Using name=$COMMIT
    NAME=$COMMIT
fi

sed -e "s/@COMMIT@/$COMMIT/g" Dockerfile.in > Dockerfile
echo docker build -t dailambda/scaml:$NAME .
docker build --squash --no-cache -t dailambda/scaml:$NAME .
rm -f app_vote.*
cp ../tests/app_vote.ml .
docker run --rm -v `pwd`:/work dailambda/scaml:$NAME /root/.opam/4.09.1/bin/scamlc app_vote.ml
rm -f app_vote.*
