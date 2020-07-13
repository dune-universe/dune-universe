| master | mlmpfr.4.1.0 | mlmpfr.4.0.2 | mlmpfr.4.0.1 | mlmpfr.4.0.0 | mlmpfr.3.1.6 |
|--------|--------------|--------------|--------------|--------------|--------------|
| [![Build Status](https://travis-ci.com/thvnx/mlmpfr.svg?branch=master)](https://travis-ci.com/thvnx/mlmpfr) | [![Build Status](https://travis-ci.com/thvnx/mlmpfr.svg?branch=release_410)](https://travis-ci.com/thvnx/mlmpfr) | [![Build Status](https://travis-ci.com/thvnx/mlmpfr.svg?branch=release_402)](https://travis-ci.com/thvnx/mlmpfr) | [![Build Status](https://travis-ci.com/thvnx/mlmpfr.svg?branch=release_401)](https://travis-ci.com/thvnx/mlmpfr) | [![Build Status](https://travis-ci.com/thvnx/mlmpfr.svg?branch=release_400)](https://travis-ci.com/thvnx/mlmpfr) | [![Build Status](https://travis-ci.com/thvnx/mlmpfr.svg?branch=release_316)](https://travis-ci.com/thvnx/mlmpfr) |

(tested on linux and osx)

# mlmpfr
OCaml bindings for MPFR.

_mlmpfr_ provides `mpfr_float`, an immutable data structure that contains a
`mpfr_t` number, as well as an optional ternary value, as provided by (and
described in) the [MPFR library](http://www.mpfr.org/).

A few distinctions are made from the original C library:

- the `mpfr_` prefix is ommited for all functions;
- `mpfr_init*` and `mpfr_set*` functions are not provided in order to implement
  these bindings with respect to the functional paradigm (and immutability).
  Consequently, `mpfr_clear*` functions are not provided too, and so, the
  garbage collector is in charge of memory management. See [initilization
  functions](https://thvnx.github.io/mlmpfr/mlmpfr/Mpfr/index.html#initialization);
- functions managing the following C types are not supported: `unsigned long
  int`, `uintmax_t`, `intmax_t`, `float`, `long double`, `__float128`,
  `_Decimal64`, `_Decimal128`, `mpz_t`, `mpq_t`, and `mpf_t`. Except for
  `mpfr_sqrt_ui` and `mpfr_fac_ui` which are partially supported in the range of
  the positive values of an OCaml signed integer. In fact, only the OCaml native
  types (signed 64-bit `int`, 64-bit `float`, and `string`) are supported. Thus,
  all functions named with `*_[su]i*` or `*_d*` are renamed here with `*_int*`
  or `*_float*`, respectively;
- bindings to functions `mpfr_*printf`, `mpfr_*random*`, `mpfr_get_patches`,
  `mpfr_buildopt_*`, and, macros `MPFR_VERSION*`, `mpfr_round_nearest_away` are
  not implemented. Everything else has been ported!

## build and install

Building *mlmpfr.4.1.0* depends on [_dune_](https://github.com/ocaml/dune) (an
OCaml build system), _ocaml_ version >= 4.04, and _MPFR_ library version 4.1.0
(see footnote for building older _mlmpfr_ releases). Basically you just need to
install _mlmpfr_ via the _opam_ package manager, which will triggers all the
dependencies (such as _dune_ for example).

- From sources (github repo or with latest
[releases](https://github.com/thvnx/mlmpfr/releases)): in _mlmpfr_ main
directory, on branch `master` or `release_410`. Make sure that you have the
proper _MPFR_ library version installed on your system because _mlmpfr_ won't
check for it (see `utils/mpfr_version.c`).

```bash
dune build @install @runtest
dune install
```

Use `--prefix <path>` to set another path (the default path is the parent of the
directory where _ocamlc_ was found).

- From _opam_, targeting latest release (see `opam info` to list available
  releases):

```bash
opam install mlmpfr
# `opam remove mlmpfr` to remove the package.
```

## documentation

Documentation depends on package [_odoc_](https://github.com/ocaml/odoc).

```bash
dune build @doc
```
then, see `_build/default/_doc/_html/index.html`. An online version
is available [here](https://thvnx.github.io/mlmpfr/index.html).

## usage

For example, let _example.ml_ as follows:

```ocaml
module M = Mpfr

let _ =
  let op = M.make_from_float (~-. 1. /. 3.) in
  Printf.printf "%s\n" (M.get_formatted_str (M.cos op))
```

Compile the above code with:

```bash
$ ocamlfind ocamlc -package mlmpfr -linkpkg example.ml -o a.out
```
will result in:

```bash
$ ./a.out
9.449569463147377e-01
```

You can also use _dune_ with

```bash
$ dune exec examples/example.exe
9.449569463147377e-01
```
----

### Note: install an older release of _mlmpfr_

Older releases of mlmpfr (3.1.6, 4.0.0, and 4.0.1) depend on
[_oasis_](http://oasis.forge.ocamlcore.org/), an obsolete build system replaced
by dune since `mlmpfr.4.0.2`. Use _opam_ if you need to install an older release
of mlmpfr, as for example:

```bash
opam install mlmpfr.3.1.6
```

Or, you can build it from the sources, as for example from branch _release_316_:

```bash
oasis setup
./configure --enable-tests
make
make test
make install
# `make uninstall` to remove mlmpfr library.
```

opam packages `mlmpfr.4.0.0` and `mlmpfr.4.0.1` also exist and are suitable for
_MPFR 4.0.0_ and _4.0.1_ versions.

### Note: build examples with an older release of _mlmpfr_

You'll need to link with `-cclib -lmpfr` (along with `ocamlopt`) if you are
using released versions of mlmpfr (as the ones provided by _opam_), since mlmpfr
wasn't linked with mpfr by default before.
