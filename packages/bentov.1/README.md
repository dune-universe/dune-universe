An OCaml implementation of histogram-sketching algorithm described in
[A Streaming Parallel Decision Tree
Algorithm](http://jmlr.org/papers/volume11/ben-haim10a/ben-haim10a.pdf)
by Yael Ben-Haim and Elad Tom-Tov .  Included is a command-line
utility `bt`, which can read a file (or stdin) containing numbers, one
per line, and output a representation of the approximated
distribution.

For example, to approximate 10 quantiles of 1M data in U(0,1):

```
echo "" | awk '{ for ( i=0 ; i < 1e6 ; i++ ) { print rand() } }' | bt -n 20 -u 10
```

In this example, the size of the approximating histogram is 20.  For
additional details, `bt --help` .

To install:

```
opam install bentov
```
