set terminal pdfcairo
set output "osc.pdf"

set grid
set title "Graph of x * sin(1/x), 227 eval"
plot "osc0.dat" with filledcurves y1=0 lt 5, "osc227.dat" with l lt 7
set title "Graph of x * sin(1/x), 389 eval"
plot "osc0.dat" with filledcurves y1=0 lt 5, "osc389.dat" with l lt 2

set title "Graph of x * sin(1/x), 227 eval"
plot "osc0.dat" with filledcurves y1=0 lt 5, \
     "osc227.dat" with p lt 7 pt 7 ps 0.15
set title "Graph of x * sin(1/x), 389 eval"
plot "osc0.dat" with filledcurves y1=0 lt 5, \
     "osc389.dat" with p lt 2 pt 6 ps 0.15


set title "Graph of sin(1/x)"
plot "osc1.dat" with filledcurves y1=0 lt 5, "osc2.dat" with l lt 1
unset title
plot "osc1.dat" with filledcurves y1=0 lt 5, \
     "osc2.dat" with p lt 1 pt 7 ps 0.15

set title "Graph of sin on [-42π, 42π]"
plot "osc3.dat" with l lt 1

set title "Graph of x ↦ sin(42x) on [-π, π]"
plot "osc4.dat" with l lt 1
