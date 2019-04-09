1.5 2019-04-06
--------------

- The library is now organized as 4 packages:
  - `interval_base` defines the module `Interval` that groups the
    functions that work on any IEEE-754 processor and offers
	basic module signatures;
  - `interval_intel`: defines a module `Interval_intel` using assembly
    instructions on Intel Processors;
  - `interval_crlibm`: defines a module `Interval_crlibm` using the
    library CRlibm to evaluate standard functions (sometimes a bit
    slower but proved enclosures in contrast to the Intel package for
    which enclosures are not always 100% correct).
  - `interval`: a meta-package that install all above three.
- `Interval.T` is a module signature to form the base of what is
  expected of any interval package.
- New functions: `invx` (extended inverse), `cancelminus`,
  `cancelplus`, `inter` `inter_exn`, `low`, `high`.
- New binary relations `equal`, `=`, `subset`, `<=`, `>=`, `precedes`,
  `interior`, `<`, `>`, `strict_precedes`, `disjoint`.
- New predicates `is_bounded`, `is_entire`.
- New constants `I.half_pi` ∋ π/2 and `I.entire` for [-∞, +∞].
- `Interval_crlibm`: functions `expm1`, `log1p`, `log2`, `log10`,
  `cospi`, `sinpi`, `tanpi`, `acospi`, `asinpi`, `atanpi`, not present
  in the Intel version.
- The module `I.U` also restores inequality relations.
- Speed and documentation improvements.

1.4 2018-03-01
--------------

- Improved interface for the `Interval` library by using sub-modules
  and standard mathematical names.  In particular, all operations —
  including infix operators — are in a sub-module `I` which can
  conveniently be used to introduce local scopes after issuing `open
  Interval`.

- Improved pretty-printing functions allowing to pass the format of
  the interval bounds.

- The library functions now signal errors by exceptions
  `Division_by_zero` and `Domain_error` that are *local* to
  `Interval`.

- The `Fpu` module has been redesigned: the rounding up or down of
  functions is controlled by the sub-module (`Low` or `High`) to which
  they belong.  This allows for natural expressions such as
  `Low.(x**2. +. 2. *. x +. 1.)`.

- Jbuilder/dune is used to compile and install the library.

- TravisCI and AppVeyor continuous integration ensure the library
  works on a variety of OCaml versions and platforms.
