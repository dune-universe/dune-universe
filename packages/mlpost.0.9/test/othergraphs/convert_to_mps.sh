#! /bin/sh

for i in mpost.[0-9]*;
do
    bn=${i%.*}
    v=${i#*.}
    cp $i $bn-$v.mps
done 
