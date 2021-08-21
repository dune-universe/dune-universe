# Benchmarks

*Caveat*: These are just some silly benchmarks I ran to make sure `Little_logger` wasn't doing anything crazy, that said, `Little_logger` is pretty snappy 🚀.  Of course, check out the benchmarking code if you're interested.

## Normal logging

* `logger` is this package
* `el` is the `easy_logging` package
* `dolog` is the `dolog` package

Here is the command I ran for the benchmarks.

```
$ dune exec ./bench/bench_little_logger.exe -- -quota 5 2> APPLE; rm APPLE
Estimated testing time 1m50s (22 benchmarks x 5s). Change using '-quota'.
┌────────────────────────┬────────────┬───────────┬──────────┬──────────┬────────────┐
│ Name                   │   Time/Run │   mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├────────────────────────┼────────────┼───────────┼──────────┼──────────┼────────────┤
│ logger_hi_s            │ 2_723.00ns │   110.00w │          │          │     33.02% │
│ logger_hi              │ 2_491.44ns │   110.00w │          │          │     30.21% │
│ logger_hi_ryan         │ 1_876.57ns │   161.00w │          │          │     22.75% │
│ logger_hi_ryan_2       │ 4_421.14ns │   234.00w │          │          │     53.61% │
│ ---------------------------------------------------------------------------------- │
│ el_hi_s                │ 5_557.40ns │   984.00w │    0.82w │    0.82w │     67.38% │
│ el_hi                  │ 5_396.78ns │ 1_013.00w │    0.92w │    0.92w │     65.44% │
│ el_hi_ryan             │ 5_511.22ns │ 1_060.00w │    0.90w │    0.90w │     66.82% │
│ el_hi_ryan_2           │ 6_195.70ns │ 1_133.00w │    0.89w │    0.89w │     75.12% │
│ ---------------------------------------------------------------------------------- │
│ dolog_hi               │ 4_630.85ns │   251.16w │          │          │     56.15% │
│ dolog_hi_ryan          │ 5_549.76ns │   287.16w │          │          │     67.29% │
│ dolog_hi_ryan_2        │ 8_247.45ns │   357.98w │          │          │    100.00% │
│ ---------------------------------------------------------------------------------- │
│ logger_hi_s_no_op      │     3.30ns │           │          │          │      0.04% │
│ logger_hi_no_op        │     3.28ns │           │          │          │      0.04% │
│ logger_hi_ryan_no_op   │     3.28ns │           │          │          │      0.04% │
│ logger_hi_ryan_2_no_op │     3.28ns │           │          │          │      0.04% │
│ ---------------------------------------------------------------------------------- │
│ el_hi_s_no_op          │    16.53ns │    28.00w │          │          │      0.20% │
│ el_hi_no_op            │    13.04ns │    10.00w │          │          │      0.16% │
│ el_hi_ryan_no_op       │    24.14ns │    20.00w │          │          │      0.29% │
│ el_hi_ryan_2_no_op     │    39.11ns │    30.00w │          │          │      0.47% │
│ ---------------------------------------------------------------------------------- │
│ dolog_hi_no_op         │     4.79ns │           │          │          │      0.06% │
│ dolog_hi_ryan_no_op    │    16.90ns │    10.00w │          │          │      0.20% │
│ dolog_hi_ryan_2_no_op  │    31.41ns │    20.00w │          │          │      0.38% │
└────────────────────────┴────────────┴───────────┴──────────┴──────────┴────────────┘
```

As you can see, `Little_logger` is pretty speedy.  It's no-op is nice and quick, with no allocations detected in the benchmark.

I wanted to compare against [Logs](https://opam.ocaml.org/packages/logs/), as it's a very nice library with the cool [continuation-passing style logger](https://discuss.ocaml.org/t/format-kprintf-usage/1396), but for some reason, it just wouldn't work on my laptop 😿.

## Logging to a file

This one is just for fun to see how writing to a file compares.  I mention in the docs, that you probably don't want to use `Little_logger` this way, but benchmarking is a fun thing to do in the evenings so ... 😹

```
$ dune exec ./bench_file_log.exe -- -quota 5
Estimated testing time 10s (2 benchmarks x 5s). Change using '-quota'.

┌─────────────────────┬──────────┬─────────┬────────────┐
│ Name                │ Time/Run │ mWd/Run │ Percentage │
├─────────────────────┼──────────┼─────────┼────────────┤
│ el_hi_ryan_file     │   5.39us │ 330.00w │    100.00% │
│ logger_hi_ryan_file │   1.08us │ 169.00w │     20.04% │
└─────────────────────┴──────────┴─────────┴────────────┘
```
