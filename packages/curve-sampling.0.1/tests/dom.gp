set terminal pdfcairo
set output "dom.pdf"
set grid

plot [-0.2:] "dom0.dat" with l lt 5, "dom.dat" with l lt 1

plot [-0.2:] "dom0.dat" with l lt 5, "dom.dat" with p lt 1 pt 6 ps 0.5

plot [-0.2:] [0:1000] "dom1.dat" with l lt 5, "dom2.dat" with l lt 1

plot [-0.2:] [0:1000] "dom1.dat" with l lt 5, \
  "dom2.dat" with p lt 1 pt 6 ps 0.6, \
  "dom3.dat" with p lt 2 pt 2 ps 0.3

set title "With viewport [0,2] × [0,3]"
plot [-0.2:] [0:4] "dom1.dat" with l lt 5 title "1/x for x > 0", \
  "dom3.dat" with p lt 2 pt 2 ps 0.5 title "with viewport"

set title "With an without viewport"
plot [-1:2] [-100:100] \
  "dom5.dat" with lp lt 1 pt 6 ps 0.5 title "no viewport", \
  "dom4.dat" with lp lt 2 pt 2 ps 0.5 title "with viewport"

set title "Heaviside function"
plot [-1:2] "dom6.dat" with l lt 5, \
  "dom7.dat" with p lt 1 pt 6 ps 0.5

set title "lngamma on [-4,8]"
plot [-4:8] [-2:15] "dom8.dat" with l lt 5 title "log |Γ(x)|", \
  "dom9.dat" with p lt 1 pt 6 ps 0.5 title "n = 203"
