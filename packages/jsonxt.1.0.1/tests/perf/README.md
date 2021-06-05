# Jsonxt performance tests

Performance tests including a comparison with yojson.
These are not built by default to avoid dependencies
on yojson, core and core\_bench.

To build first install yojson, core and core\_bench then
run

```
dune build @perf_tests
```

## perf\_float
This tests the performance of the Jsonxt floating point string conversion,
which handles integers-in-floats as integers, versus the standard float
formating.

Two options are supported: `int` and `float`. `int` performs
a performance comparison when converting an integer in a
float to a string. `float` performs the same test with a 
floating point number.

The results show
- intopt: the integer optimised conversion
- float: using the format\_float library function
- printf: using Printf.printf

## perf\_yojson\_of\_string
This compares Jsonxt's and Yojson's parsing of strings

## perf\_yojson\_to\_string
This compares Jsonxt's and Yojson's writing of strings
