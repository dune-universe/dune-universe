# OCaml implementation of BLS12-381

## Encoding

### Scalar

The scalar field is `Fr = GF(0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001)`, encoded on 32 bytes in little endian.

### Groups

For G1, the base field is `Fq:
GF(0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab`)
and `E(Fq) := y^2 = x^3 + 4`. An element of the base field can be encoded on 48 bytes (using only
381 bits, leaving 3 bits unused).

For G2, the base field is `Fq2 := Fq[Z]/(X^2 + 1)` and `E(Fq2) := y^2 = x^3 + 4
(Z + 1)`. An element of the base field can be encoded on 2 * 48 bytes
representing each coefficient of the polynomial. 3 bits of each coefficient
encoding are unused.

The « uncompressed » form `(x, y)` of G1 and G2 is the concatenation of the elements `x` and `y` encoded in big endian.

The « compressed » form uses the first 3 most significant (and unused) bits of
the coordinate `x`.
- the first most significant bit is always set to `1` to carry the information it
is the compressed encoding of a point.
- the second most significant bit is set to `1` if the element is the identity of the curve.
- the third most significant bit is the sign of `y`. It is set to `1` if `y` is
  lexicographically larger than `-y`.

## Install


```shell
# if you implement a library and you don't need an actual implementation
opam install bls12-381
# to target UNIX
opam insall bls12-381-unix
# to target JavaScript, to be used with jsoo.
opam install bls12-381-js
```

See below how to use in your project.

## Run tests

```
dune runtest
dune build @test/js/browser/serve # Check _build/default/test/js/browser and run `npm run serve`
```

To get the coverage (only ok for bls12-381-unix and bls12-381-gen)
```
dune runtest --instrument-with bisect_ppx --force
bisect-ppx-report html
```

## How to use in my project

If you are developing a library using `bls12-381`, you only need to add `bls12-381` in the dependency list.
However, if you are writing a binary, three packages are relevant:
- `bls12-381-unix`: to be used for UNIX
- `bls12-381-js`: to be used to target JavaScript. It does rely on the node
  packages listed
  [here](https://gitlab.com/dannywillems/rustc-bls12-381/-/packages), version >=
  0.8.1, and suppose this module is loaded before in the global namespace under
  the name `_RUSTC_BLS12_381`. See `test/js/browser/` for an example. You need
  to use the appropriate package depending on the platform you target (Node or a
  bundler for the browser). For instance, if you target Node (resp. a bundler
  like webpack for a browser usage), you must use
  `@dannywillems/rustc-bls12-381-node` (resp. `@dannywillems/rustc-bls12-381`).
- `bls12-381-js-gen`: if the module is loaded somewhere else than in the global
  namespace like the previous package supposes, you can use this third library
  that provides functors. The functors are expecting a module of the signature:
  ```ocaml
  sig
    val rust_module : unit -> Jsoo_lib.ESModule.t

    val get_wasm_memory_buffer : unit -> Jsoo_lib.Memory.Buffer.t
  end
  ```
  (note: functions are not directly evaluated values are required because the module might not already be loaded when loading the JavaScript resulting file and the wasm memory buffer being a view over a ArrayBuffer might be invalid if the buffer size is increased at runtime). You can see an example in the `src/js`.

For more examples, see the test directories.

## Run the benchmarks

```
opam install core_bench
dune exec benchmark/bench_fr.exe
dune exec benchmark/bench_g1.exe
dune exec benchmark/bench_g2.exe
dune exec benchmark/bench_pairing.exe
dune exec benchmark/bench_signature.exe
```

## Documentation

```
opam install odoc
dune build @doc
```
