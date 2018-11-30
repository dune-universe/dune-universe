set terminal pdfcairo
set output "nice.pdf"

set grid
set title "Graph of a nice parametric curve"
plot "nice0.dat" with l lt 2, "nice1.dat" with lp lt 1 pt 6 ps 0.5

set title "exp(-x²), n=53"
plot "nice2.dat" with l lt 5 lw 3, "nice3.dat" with lp lt 7 pt 6 ps 0.5
