# 1.2.0pre7

## Features

* Based on OCaml 4.09.1
* BREAKING CHANGE: First implementation of multi unit compilation.  [@entry] is now mandatory for entry points.
  This is NOT separate compilation, therefore just "multi unit compilation".  Now `scamlc` can take more than one `*.ml` file and compile them to one `*.tz` file.
* Use of typerep for interoperability between Michelson and OCaml values.
* Right balanced value layout with optimization for nullary constructors.  Note: this may be incompatible with other high level languages for Michelson.

## Optimization

* New optimization using K normal form.  Optimization complexity issue was resolved.
* `dip_1_drop_n_compaction` is now applied to opcodes in constants

## Primitives

* SCaml.ml has more implementations of primitives.
* Added `Option.value`, `Option.get`, `Sum.get_left`, `Sum.get_right`
* Added `List.rev_append`
* Added `SCaml.Contract.contract'`, address to contract conversion with an entry point name
* Added `SCaml.raise` for better exception simulation in OCaml
* Added `SCaml.Obj.pack'` and `SCaml.Obj.pack'` with Typerep arguments.

## Bug fixes

* Argument order of List.fold_left was fixed

## Others

* Directory reorganization
* OCaml Michelson converison using `ppx_typerep_conv`

# 1.1.1

* Fixed a bug of constructor mending for bool in pattern match compilation (#49)
* Fixed a bug in optimization.
* `big_map` constants for `--scaml-convert*`
* Fixed indentation of `--scaml-convert*` output
* Add options `--scaml-convert-value` and `--scaml-convert-type` (#41) (Haochen Xie)

# 1.1.0

* `scamlc -v` now prints out SCaml library directory
* Applied `clean_failwith` against LAMBDA body
* Added the default case for nullary constructor pattern matches (#40)
* Added `(^)`, `/`, `/^`, `/$` and `/$^` to `SCaml`
* Fixed printing of IML
* Fixed type property name: "storable" => "packable"
* Check of storage type
* Changed the environment variable name: `SCAMLLIB` => `SCAMLIB`
* SCaml error IDs.
* Added `--scaml-revert`, to revert SCaml expression from a Michelson value and its SCaml type
* `.merlin` for SCaml examples
* Docker image support

# 1.0.3

* Field and constructor annotations in output Michelson code
* Very rough implementation of --scaml-revert, which is to revert SCaml expression from a Michelson value and its SCaml type.
* Checks for comparable, packable, and parameterable types.
* Fixed the compilation of SELF by introducing Annot "not_expand" not to expand let-def.
* Michelson optimziation : DIP { DROP } x n => DIP { DROP n }
* Introduce docker file

# 1.0.2

* Fixed typos of messages
* Fixed OPAM version constraints

# 1.0.1

## Language

* Prevent non storables from being `APPLY`'ed to `LAMBDA`.
    * `fun` body can no longer have free variable occurrences 
	   with non storable types.
    * Stopped reducing  let x = e1 in e2  =>  e2[e1/x]  since it may change 
	  free variable occurrences inside fun, which may put unserializable 
	  values into closures.
* Added missing optimizations
* IML printing by Pprintast using ppxlib
* Removing garbages after FAILWITH in constants

## Library

* Added `List.fold_left'`, `Set.fold'`, `Map.fold'`, `Map.map'`, variants of
  `fold` and `map` which take uncurried functions.

## Tests

* Run typecheck before run
* Multisig examples
* Doc and fix for `test.sh` and `test_all.sh`

# 1.0.0

Initial release
