#!/bin/bash

./_build/default/src/example.exe 90 > toplot

gnuplot -presist <<EOF
set ticslevel 0
splot 'toplot' with lines
EOF
