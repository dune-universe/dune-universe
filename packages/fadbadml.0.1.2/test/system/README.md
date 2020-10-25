## TEST

```
make test
```

or to test specific modules

```
make; ./compare.py MOD
```

where `MOD` is either `fad`, `bad`, `fad_bad` or `tad`

run `./compare.py -h` to see the optional arguments

## BENCH

```
make; ./bench.py MOD -plot
```

where `MOD` is either `fad`, `bad`, `fad_bad` or `tad`

run `./bench.py -h` to see the optional arguments
