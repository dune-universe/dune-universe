## Benchmark

This directory provides a functor to benchmark basic operation on finite field,
see `bench_ff.ml`. `Core_bench` is used for the benchmark tool.
Here how to use:

```ocaml
module F337 = Ff.Make (struct let prime_order = Z.of_string "337" end)
module BenchmarkF337Uncompressed = ECBenchmark (F337)
let () =
  let commands = BenchmarkG1Uncompressed.get_benches "F337" in
  Core.Command.run (Bench.make_command commands)
```

### Run the benches

```shell
opam install core_bench.v0.12.0
dune exec benchmark/bench_ff.exe
```
