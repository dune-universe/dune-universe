#!/bin/sh

SYSTEM=$1
CMD=$2
ARG1=$3
ARG2=$4
ARG3=$5

if test "${SYSTEM}" = "mingw"; then
  OCAMLFIND_TOOLCHAIN=windows wine $CMD $ARG1 $ARG2 $ARG3
elif test "${SYSTEM}" = "mingw64"; then
  OCAMLFIND_TOOLCHAIN=windows wine64 $CMD $ARG1 $ARG2 $ARG3
else
  $CMD $ARG $ARG1 $ARG2 $ARG3
fi
