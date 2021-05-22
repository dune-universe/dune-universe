# BENCH

Some simple early benchmarking mostly to determine if results within OCaml match
those purely using the C example.


Here we use the [hey](https://github.com/rakyll/hey) http load generator because I have it
and it seems to work pretty well. You could also use wrk2 if you prefer that.

## Most recent benchmarking run
Again using the same `hey` command but with updated upstream library and now on a shiny new 2021 Macbook pro.
```
$ hey -n 20000 -c 20 http://127.0.0.1:8080/plaintext

Summary:
  Total:	0.2864 secs
  Slowest:	0.0024 secs
  Fastest:	0.0001 secs
  Average:	0.0003 secs
  Requests/sec:	69840.5078

  Total data:	260000 bytes
  Size/request:	13 bytes

Response time histogram:
  0.000 [1]	|
  0.000 [15951]	|■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  0.001 [2556]	|■■■■■■
  0.001 [887]	|■■
  0.001 [489]	|■
  0.001 [87]	|
  0.001 [22]	|
  0.002 [3]	|
  0.002 [3]	|
  0.002 [0]	|
  0.002 [1]	|


Latency distribution:
  10% in 0.0002 secs
  25% in 0.0002 secs
  50% in 0.0002 secs
  75% in 0.0003 secs
  90% in 0.0004 secs
  95% in 0.0007 secs
  99% in 0.0009 secs

Details (average, fastest, slowest):
  DNS+dialup:	0.0000 secs, 0.0001 secs, 0.0024 secs
  DNS-lookup:	0.0000 secs, 0.0000 secs, 0.0000 secs
  req write:	0.0000 secs, 0.0000 secs, 0.0007 secs
  resp wait:	0.0002 secs, 0.0000 secs, 0.0008 secs
  resp read:	0.0000 secs, 0.0000 secs, 0.0014 secs

Status code distribution:
  [200]	20000 responses
```


## Initial run
Ages ago.
```
$ hey -n 20000 -c 20 http://127.0.0.1:8080/plaintext
Summary:
  Total:	0.8547 secs
  Slowest:	0.0089 secs
  Fastest:	0.0001 secs
  Average:	0.0008 secs
  Requests/sec:	23400.6257
  Total data:	260000 bytes
  Size/request:	13 bytes

Response time histogram:
  0.000 [1]	|
  0.001 [15857]	|∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎
  0.002 [2909]	|∎∎∎∎∎∎∎
  0.003 [846]	|∎∎
  0.004 [250]	|∎
  0.005 [64]	|
  0.005 [17]	|
  0.006 [18]	|
  0.007 [27]	|
  0.008 [3]	|
  0.009 [8]	|

Latency distribution:
  10% in 0.0004 secs
  25% in 0.0005 secs
  50% in 0.0006 secs
  75% in 0.0009 secs
  90% in 0.0015 secs
  95% in 0.0020 secs
  99% in 0.0033 secs

Details (average, fastest, slowest):
  DNS+dialup:	 0.0000 secs, 0.0000 secs, 0.0078 secs
  DNS-lookup:	 0.0000 secs, 0.0000 secs, 0.0000 secs
  req write:	 0.0000 secs, 0.0000 secs, 0.0024 secs
  resp wait:	 0.0007 secs, 0.0001 secs, 0.0068 secs
  resp read:	 0.0001 secs, 0.0000 secs, 0.0027 secs

Status code distribution:
  [200]	20000 responses
```
