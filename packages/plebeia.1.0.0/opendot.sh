#!/bin/sh

pdf="$1.pdf"

dot -Tpdf -o"$pdf" $1
open $pdf
