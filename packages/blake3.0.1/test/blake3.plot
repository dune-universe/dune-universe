set xlabel "input size"
set ylabel "time (microseconds)"
set logscale x
plot "z.data" using 1:3 with line title "blake3multi", \
     "z.data" using 1:4 with line title "blake3single", \
     "z.data" using 1:5 with line title "blake2"




