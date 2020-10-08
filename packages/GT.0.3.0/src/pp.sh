#!/bin/bash

if [ "${1##*.}" == "mli" ] ; then
  camlp5o pr_o.cmo ../camlp5/pa_gt.cmo $1
else
  m4 macro.m4 $1 > $1.pp
  camlp5o pr_o.cmo ../camlp5/pa_gt.cmo -impl $1.pp
fi
