set terminal pdfcairo
set output "clip_gp.pdf"

set grid
set title "Path clipped to [0,1]²"
plot "clip0.dat" with l lt 1, "clip1.dat" with l lt 6 lw 3

set title "Vertical asymptote at x=1/4"
plot [-0.5:5.5] [-4:2] "clip2.dat" with filledcurves y1=0 lt 5, \
  "clip3.dat" with l lt 1, \
  "clip4.dat" with l lt 6 lw 3

unset title
plot [-0.5:5.5] [-10:2] "clip2.dat" with filledcurves y1=0 lt 5, \
  "clip3.dat" with p lt 1 pt 7 ps 0.2
