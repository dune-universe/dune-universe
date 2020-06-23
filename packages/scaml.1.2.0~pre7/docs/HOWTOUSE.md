# How to use SCaml

## Learn OCaml

SCaml is a modified OCaml compiler.
If you are not familiar with OCaml, please learn it first.

## Modes

### Compilation mode: `scamlc`

The compiler `scamlc` has almost the same interface as `ocamlc`.
`scamlc xxx.ml` compiles `xxx.ml` to `xxx.tz`.

### Conversion mode: `scamlc --scaml-convert`

SCaml specific compiler switch `--scaml-convert`.
With this option, `scamlc` command takes a `.ml` and print Michelson representations of
ML constants and types to stdout.  The conversion targets must be defined as toplevel
declarations.  For example:

```ocaml
(* hoo.ml *)
open SCaml
type t = 
  { name   : string
  ; age    : nat
  ; salary : tz
  }

and u = 
   | Foo of int * tz * string
   | Bar
   | Boo of t list
   | Far

let v = Boo [ { name= "jon"; age= Nat 18; salary= Tz 10000.0 }
            ; { name= "dow"; age= Nat 50; salary= Tz 1.0 }
            ]
```

then,

```shell
$ scamlc --scaml-convert hoo.ml
type t: pair string (pair nat mutez)
type u: or int (or (pair int (pair mutez string)) (list (pair string (pair nat mutez))))
v: Right (Right { Pair "jon" (Pair 18 10000000000) ; Pair "dow" (Pair 50 1000000) })
```

Note that the values must be constants.  Constructors and types can refer to types
defined in other modules, as far as they are already compiled to `.cmi` files.

### Revert mode: `scamlc --scaml-revert file`

SCaml specific compiler switch `--scaml-revert file`, where `file` is a file name 
which contains Michelson constant expression.

With this option, `scamlc` command takes a `.ml` of one type definition, then translate
the Michelson constant expression in `file` as an SCaml value of the type in `.ml`,
then print out the SCaml expression to stdout.  For example, suppose we have `hoo.ml`
of the example of `--scaml-convert`.  Create `hoo_type.ml` with a type alias definition
of `Hoo.t`:

```ocaml
(* hoo_u.ml *)
type u = Hoo.u (* Refers the type defined in hoo.ml in the example of --scaml-convert *)
```

Prepare a file with the Michelson constant obtained in the example of the conversion:
```
/* value.tz */
Right (Right { Pair "jon" (Pair 18 10000000000) ; Pair "dow" (Pair 50 1000000) })
```

```shell
$ scamlc --scaml-revert value.tz hoo_u.ml
Boo
  [{ name = "jon"; age = (Nat 18); salary = (Tz 10000.000000) };
  { name = "dow"; age = (Nat 50); salary = (Tz 1.000000) }]
```

Note that the values must be constants.  Constructors and types can refer to types
defined in other modules, as far as they are already compiled to `.cmi` files.

## Where to find "SCamlib" module

To compile smart contracts, you need to use `SCaml` module which declares the primitives for them.  The module `SCaml` is seeked in directories in the following order:

* In the current directory.
* Directories specified by `-I <dir>` option.
* If `--scaml-noscamlib` is not specified:
    * Directory specified by the environment variable `SCAMLIB`.
	* If `SCAMLIB` is not speicified, directory `` `opam config var prefix`/lib/scaml``.
