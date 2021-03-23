## Benchmark

This directory contains benchmarks for G1, G2, Fr, Fq12 and the pairing function.
The benchmarks are generic (using functors), and can be used for any elliptic
curves or finite fields. `Core_bench` is used for the benchmark tool.

For the elliptic curves (resp. finite fields), a functor `ECBenchmark` (resp.
`FFBenchmark`) is provided in `bench_ec.ml` (resp. `bench_ff.ml`).

Benchmarking a finite field or an elliptic curve implementation is relatively simple using these functors:

```ocaml
module BenchmarkG1Uncompressed = ECBenchmark (Bls12_381.G1.Uncompressed)
let () =
  let commands = BenchmarkG1Uncompressed.get_benches "G1 Uncompressed" in
  Core.Command.run (Bench.make_command commands)
```

### Run the benches

```shell
opam install core_bench.v0.12.0
dune exec benchmark/bench_ec.exe
dune exec benchmark/bench_ff.exe
dune exec benchmark/bench_pairing.exe
```

### Benchmarks results

The following results have been run on a machine with
- CPU: Intel(R) Core(TM** i7-8565U CPU @ 1.80GHz
- RAM: 16Go

and with the commands given above.
Benchmark of the Rust code (not including the conversion between C array and the Rust representation) can be found [here](https://gitlab.com/dannywillems/rustc-bls12-381/-/tree/master/src%2Fbenchmark).

**!!Note benchmarking depends on the machine and may differ between execution.
Sometimes, we experienced a difference of 10%. For instance, we have experienced
a time/run of 2.78ms for the pairing!!**

#### Elliptic curves

| Name                                                                                    | Rust 1.40.0 only | Rust 1.40.0, OCaml 4.07.1 | mWd/Run |
|-----------------------------------------------------------------------------------------|------------------|---------------------------|---------|
| G1 from uncompressed to projective                                                      | 212,832ns        | NA                        | NA      |
| G1 from uncompressed to affine                                                          | 217,667ns        | NA                        | NA      |
| G1 from uncompressed to unchecked affine                                                | 140ns            | NA                        | NA      |
| G1 from affine to projective                                                            | 5ns              | NA                        | NA      |
| G1 from projective to affine                                                            | 16ns             | NA                        | NA      |
| G2 from uncompressed to projective                                                      | 742,376ns        | NA                        | NA      |
| G2 from uncompressed to affine                                                          | 736,388ns        | NA                        | NA      |
| G2 from uncompressed to unchecked affine                                                | 290ns            | NA                        | NA      |
| G2 from affine to projective                                                            | 16ns             | NA                        | NA      |
| G2 from projective to affine                                                            | 26ns             | NA                        | NA      |
| -------------------------                                                               | --               | -                         | -       |
| G1 Uncompressed compute addition pregenerated random element                            |                  | 7_686.72ns                | 14.00w  |
| G1 Uncompressed random generation                                                       |                  | 165_477.34ns              | 14.00w  |
| G1 Uncompressed zero generation                                                         |                  | 35.81ns                   | 14.00w  |
| G1 Uncompressed one generation                                                          |                  | 133.02ns                  | 14.00w  |
| G1 Uncompressed check if zero on pregenerated random                                    |                  | 163.92ns                  |         |
| G1 Uncompressed check if zero on pregenerated one                                       |                  | 161.79ns                  |         |
| G1 Uncompressed check if zero on pregenerated zero                                      |                  | 50.23ns                   |         |
| G1 Uncompressed check equality on random                                                |                  | 342.32ns                  |         |
| G1 Uncompressed check equality on one and random                                        |                  | 363.79ns                  |         |
| G1 Uncompressed check equality on zero and random                                       |                  | 225.09ns                  |         |
| G1 Uncompressed check equality on same element                                          |                  | 356.92ns                  |         |
| G1 Uncompressed compute scalar multiplication on pregenerated random element and scalar |                  | 254_920.14ns              | 14.00w  |
| G1 Uncompressed opposite of pregenerated random element                                 |                  | 315.45ns                  | 14.00w  |
| G1 Uncompressed opposite of zero                                                        |                  | 76.45ns                   | 14.00w  |
| G1 Uncompressed opposite of one                                                         |                  | 323.75ns                  | 14.00w  |
| G1 Compressed compute addition pregenerated random element                              |                  | 72_450.73ns               | 8.00w   |
| G1 Compressed random generation                                                         |                  | 179_380.93ns              | 8.00w   |
| G1 Compressed zero generation                                                           |                  | 40.13ns                   | 8.00w   |
| G1 Compressed one generation                                                            |                  | 160.00ns                  | 8.00w   |
| G1 Compressed check if zero on pregenerated random                                      |                  | 32_034.74ns               |         |
| G1 Compressed check if zero on pregenerated one                                         |                  | 32_410.89ns               |         |
| G1 Compressed check if zero on pregenerated zero                                        |                  | 44.06ns                   |         |
| G1 Compressed check equality on random                                                  |                  | 65_869.80ns               |         |
| G1 Compressed check equality on one and random                                          |                  | 64_299.25ns               |         |
| G1 Compressed check equality on zero and random                                         |                  | 31_490.99ns               |         |
| G1 Compressed check equality on same element                                            |                  | 62_391.18ns               |         |
| G1 Compressed compute scalar multiplication on pregenerated random element and scalar   |                  | 313_714.56ns              | 8.00w   |
| G1 Compressed opposite of pregenerated random element                                   |                  | 30_238.74ns               | 8.00w   |
| G1 Compressed opposite of zero                                                          |                  | 67.39ns                   | 8.00w   |
| G1 Compressed opposite of one                                                           |                  | 30_523.33ns               | 8.00w   |
| G2 Uncompressed compute addition pregenerated random element                            |                  | 12_333.90ns               | 26.00w  |
| G2 Uncompressed random generation                                                       |                  | 1_810_567.06ns            | 26.00w  |
| G2 Uncompressed zero generation                                                         |                  | 64.04ns                   | 26.00w  |
| G2 Uncompressed one generation                                                          |                  | 282.05ns                  | 26.00w  |
| G2 Uncompressed check if zero on pregenerated random                                    |                  | 342.08ns                  |         |
| G2 Uncompressed check if zero on pregenerated one                                       |                  | 346.60ns                  |         |
| G2 Uncompressed check if zero on pregenerated zero                                      |                  | 108.97ns                  |         |
| G2 Uncompressed check equality on random                                                |                  | 691.24ns                  |         |
| G2 Uncompressed check equality on one and random                                        |                  | 677.22ns                  |         |
| G2 Uncompressed check equality on zero and random                                       |                  | 740.30ns                  |         |
| G2 Uncompressed check equality on same element                                          |                  | 818.85ns                  |         |
| G2 Uncompressed compute scalar multiplication on pregenerated random element and scalar |                  | 900_057.42ns              | 26.00w  |
| G2 Uncompressed opposite of pregenerated random element                                 |                  | 624.82ns                  | 26.00w  |
| G2 Uncompressed opposite of zero                                                        |                  | 150.71ns                  | 26.00w  |
| G2 Uncompressed opposite of one                                                         |                  | 618.24ns                  | 26.00w  |
| G2 Compressed compute addition pregenerated random element                              |                  | 511_858.64ns              | 14.00w  |
| G2 Compressed random generation                                                         |                  | 3_966_696.57ns            | 14.00w  |
| G2 Compressed zero generation                                                           |                  | 64.88ns                   | 14.00w  |
| G2 Compressed one generation                                                            |                  | 231.83ns                  | 14.00w  |
| G2 Compressed check if zero on pregenerated random                                      |                  | 214_350.61ns              |         |
| G2 Compressed check if zero on pregenerated one                                         |                  | 231_000.20ns              |         |
| G2 Compressed check if zero on pregenerated zero                                        |                  | 67.94ns                   |         |
| G2 Compressed check equality on random                                                  |                  | 447_347.83ns              |         |
| G2 Compressed check equality on one and random                                          |                  | 477_699.12ns              |         |
| G2 Compressed check equality on zero and random                                         |                  | 235_987.47ns              |         |
| G2 Compressed check equality on same element                                            |                  | 470_452.31ns              |         |
| G2 Compressed compute scalar multiplication on pregenerated random element and scalar   |                  | 1_150_434.48ns            | 14.00w  |
| G2 Compressed opposite of pregenerated random element                                   |                  | 227_593.55ns              | 14.00w  |
| G2 Compressed opposite of zero                                                          |                  | 107.44ns                  | 14.00w  |
| G2 Compressed opposite of one                                                           |                  | 217_461.02ns              | 14.00w  |

#### Finite field

| Name                                                    | Rust 1.40.0 | Rust 1.40.0, OCaml 4.07.1 | mWd/Run |
|---------------------------------------------------------|-------------|---------------------------|---------|
| Fr compute addition pregenerated random element         |             | 129.94ns                  | 6.00w   |
| Fr random generation                                    |             | 69.24ns                   | 6.00w   |
| Fr zero generation                                      |             | 40.96ns                   | 6.00w   |
| Fr one generation                                       |             | 33.38ns                   | 6.00w   |
| Fr check if zero on pregenerated random                 |             | 41.71ns                   |         |
| Fr check if zero on pregenerated one                    |             | 41.56ns                   |         |
| Fr check if zero on pregenerated zero                   |             | 43.14ns                   |         |
| Fr check if one on pregenerated random                  |             | 43.89ns                   |         |
| Fr check if one on pregenerated one                     |             | 44.04ns                   |         |
| Fr check if one on pregenerated zero                    |             | 43.81ns                   |         |
| Fr compute addition on pregenerate random               |             | 119.29ns                  | 6.00w   |
| Fr compute multiplication on pregenerate random         |             | 165.85ns                  | 6.00w   |
| Fr compute square on pregenerate random                 |             | 103.47ns                  | 6.00w   |
| Fr compute double on pregenerate random                 |             | 83.34ns                   | 6.00w   |
| Fr compute equality on random                           |             | 81.68ns                   |         |
| Fr compute equality on same element                     |             | 80.71ns                   |         |
| Fr compute opposite of pregenerated random element      |             | 85.49ns                   | 6.00w   |
| Fr compute opposite of pregenerated one element         |             | 84.49ns                   | 6.00w   |
| Fr compute opposite of pregenerated zero element        |             | 81.65ns                   | 6.00w   |
| Fr compute inverse of pregenerated random element       |             | 2_621.53ns                | 6.00w   |
| Fr compute inverse of pregenerated one element          |             | 2_404.37ns                | 6.00w   |
| Fr compute inverse opt of pregenerated random element   |             | 2_492.41ns                | 8.00w   |
| Fr compute inverse opt of pregenerated one element      |             | 2_462.46ns                | 8.00w   |
| Fr compute inverse opt of pregenerated zero element     |             | 43.72ns                   |         |
| Fq12 compute addition pregenerated random element       |             | 2_364.19ns                | 74.00w  |
| Fq12 random generation                                  |             | 1_167.39ns                | 74.00w  |
| Fq12 zero generation                                    |             | 618.07ns                  | 74.00w  |
| Fq12 one generation                                     |             | 622.53ns                  | 74.00w  |
| Fq12 check if zero on pregenerated random               |             | 817.53ns                  |         |
| Fq12 check if zero on pregenerated one                  |             | 789.68ns                  |         |
| Fq12 check if zero on pregenerated zero                 |             | 799.72ns                  |         |
| Fq12 check if one on pregenerated random                |             | 888.10ns                  |         |
| Fq12 check if one on pregenerated one                   |             | 888.75ns                  |         |
| Fq12 check if one on pregenerated zero                  |             | 828.30ns                  |         |
| Fq12 compute addition on pregenerate random             |             | 2_591.27ns                | 74.00w  |
| Fq12 compute multiplication on pregenerate random       |             | 10_111.69ns               | 74.00w  |
| Fq12 compute square on pregenerate random               |             | 4_304.30ns                | 74.00w  |
| Fq12 compute double on pregenerate random               |             | 1_570.81ns                | 74.00w  |
| Fq12 compute equality on random                         |             | 1_559.72ns                |         |
| Fq12 compute equality on same element                   |             | 1_622.36ns                |         |
| Fq12 compute opposite of pregenerated random element    |             | 1_599.65ns                | 74.00w  |
| Fq12 compute opposite of pregenerated one element       |             | 1_536.35ns                | 74.00w  |
| Fq12 compute opposite of pregenerated zero element      |             | 1_520.81ns                | 74.00w  |
| Fq12 compute inverse of pregenerated random element     |             | 15_882.21ns               | 74.00w  |
| Fq12 compute inverse of pregenerated one element        |             | 15_079.48ns               | 74.00w  |
| Fq12 compute inverse opt of pregenerated random element |             | 15_682.97ns               | 76.00w  |
| Fq12 compute inverse opt of pregenerated one element    |             | 16_443.32ns               | 76.00w  |
| Fq12 compute inverse opt of pregenerated zero element   |             | 828.00ns                  |         |

#### Pairing

| Name                                                                                                       | Rust 1.40.0 | Rust 1.40.0, OCaml 4.07.1 | mWd/Run | mjWd/Run | Prom/Run |
|------------------------------------------------------------------------------------------------------------|-------------|---------------------------|---------|----------|----------|
| Pairing on pregenerated uncompressed random elements                                                       |             | 1_953.02us                | 74.00w  |          |          |
| Miller loop on pregenerated uncompressed random elements                                                   |             | 620.66us                  | 74.00w  |          |          |
| Miller loop on three pregenerated couples of uncompressed random elements                                  |             | 2_710.96us                | 304.00w | 0.17w    | 0.17w    |
| Miller loop on three pregenerated couples of uncompressed random elements followed by final exponentiation |             | 4_267.86us                | 380.00w | 0.14w    | 0.14w    |
| Final exponentiation on pregenerated random element                                                        |             | 1_508.32us                | 76.00w  |          |          |
