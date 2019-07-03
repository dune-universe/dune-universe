#!/bin/bash

\rm -f _build/default/src/test_camltc.exe
jbuilder build src/test_camltc.exe
_build/default/src/test_camltc.exe
