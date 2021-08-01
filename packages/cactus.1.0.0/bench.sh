export F=50
export N=1_000_000
export S=50_000_000
if dune exec -- ./bench/bench.exe --minimal --clear --fanout $F --page 5000 -n $N --with-profiling -vv --size $S ; then
  python3 bench/graph.py _bench/replace_random --with-log
fi