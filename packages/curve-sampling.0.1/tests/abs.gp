set terminal pdfcairo
set output "abs.pdf"
set grid
set y2tics

plot "abs0.dat" with l lt 5 title "function", \
  "abs.dat" with l lt 1 title "n = 40"

plot "abs0.dat" with l lt 5 title "function", \
  "abs.dat" with p lt 1 pt 6 ps 0.5 title "n = 40", \
  "abs_s.dat" using 1:8 with lp ps 0.2 lt rgb "#3f3f3f" axes x1y2 \
    title "cost segments"

plot "abs1.dat" with l lt 5 title "|sin x|", \
  "abs2.dat" with l lt 1 title "n = 50"
plot "abs1.dat" with l lt 5 title "|sin x|", \
  "abs2.dat" with p lt 1 pt 6 ps 0.5 title "n = 50", \
  "abs2_s.dat" using 1:8 with lp ps 0.2 lt rgb "#3f3f3f" axes x1y2 \
    title "cost segments"

