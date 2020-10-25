set terminal svg
set output "picture.svg"

set xrange [0:7]
set yrange [0:4]

plot "data.dat" using 1:2:-2 w l title "Overapprox" lc variable
