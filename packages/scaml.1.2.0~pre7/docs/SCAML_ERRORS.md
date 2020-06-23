# SCaml error codes

SCaml is a restricted version of OCaml.  In addition to the ordinary OCaml
parsing and typing errors (consult OCaml documents for these OCaml errors), SCaml
may reject your code with its own errors.

SCaml errors are prefixed with `[ESCaml<error code>]`.  Do *not* consult OCaml documents for these SCaml specific errors.  Here are the list of SCaml errors.

## 000 Unsupported OCaml feature

SCaml does not support that OCaml language feature.

### Reason

SCaml team has good reasons not to support it:  it could be too hard to give a proper compilation to Michelson.  Or, it would not be a very essential language feature.

### Workaround

The best way to avoid this error is not to use that language feature.

## 100 Unsupported type

A type expression explicitly written, of an expression, or of a pattern is unsupported or invalid in SCaml.

### Workaround

Do not use that type construction.

## 200 Constant error

Invalid use of smart contract related data constructors which can only take constants.

### Reason

Smart contract related data constructors such as `Int`, `Address`, `Set` etc can only take constant expressions.  Some of them can only take some specific form of data: for example, `Timestamp` can only take a constant string in the RFC3339 time representation.

## 300 Entry point

Something wrong has been found in the form of entry points.  Possibilities:

* There exists no entry point.
* Problem found in one of `[@entry]` attributes.

## 310 Entry point typing error

One of the entry points failed to type to make a program valid as a smart contract.

### Reason

Several conditions must hold:

* The entry points in a contract must have the form:
  `(param, storage) SCaml.entry` for some type `param` and `storage`,
  which is equal to `param -> storage -> SCaml.operations * storage`.
* If there are multiple entry points, they must share the same storage type.
* Some type constructors cannot appear in `param` and `storage` types.
    * `param` cannot contain `SCaml.operation`.
	* `storage` cannot contain `SCaml.operation` and `SCaml.contract`.

## 400 Free variable

This is the most non intuitive SCaml restriciton.

SCaml's functions do not allow free variable occurrences of non packable types (note: older Michelson documents refer them as "unstorable") in their bodies:

* Non packable types are `SCaml.big_map`, `SCaml.operation`, `SCaml.contract`, and types containing these types.
* The free variable occurrences of a function are variables defined outside of the function body.  For example, `fun x -> (x, y, let z = 1 in z)`, `y` is the sole free variable occurrence of the function; `x` is bound by the function abstractrion and `z` is locally defined.

This may sound strange but is forced by the same restriction of Michelson's function closures.

The restriction is *not* applied to the entry point arguments; they are handled without Michelson closures.

### Uncurried primitives

This restriction is troublesome especially when you use higher order functions
like `List.fold_left` with non packable types.  Here is an *invalid* expression example which tries to send tokens to each member of a contract list:

```
List.fold_left
	(fun ops -> fun c -> Operation.transfer_tokens () (Tz 1.0) c :: ops)
	[] contracts
```

This is rejected by SCaml, since `ops` freely occurring in the inner function
`fun c -> ..`.  This is troublesome since this kind of code is quite typical
in smart contract programming.

To work around this specific problem, SCaml provides another version of list folding `List.fold_left'`:

```
List.fold_left'
	(fun (ops, c) -> Operation.transfer_tokens () (Tz 1.0) c :: ops)
	[] contracts
```

Here, the two function abstractions in the previous example are "uncurried" i.e. squashed into one which takes a tuple `(ops, c)`, thus making the occurrence of `ops` is no longer free.  SCaml provides this kind of uncurried foldings for `list`, `set`, and `map`.

### General workaround

The principle is to avoid free occurrences of non packable variables.  There are several ways:

* Use `SCaml.key_hash` or `SCaml.address` instead of `SCaml.contract` if possible.
* Use uncurried functions.

## 500 Self

## 510 Self typing error

## 600 Invalid contract creation

Error around `Contract.create_*`.  They have the following restrictions:

* They must be fully applied.
* Code text and file name of the contract source must be in constant.


## 700 Pattern match error

SCaml introduces some additional restrictions to pattern matching:

* SCaml rejects non exhaustive pattern matches.
* Conversion mode does not support complex patterns.

## 800 Primitive

An illegal use of a SCaml primitive is found:

* Comparison primitives only take comparable types.
* Packing primitives only take packable types.

## 900 Flags

Compiler flags conflict each other.

## 910 Attribute error

The problem happened around SCaml's attributes.

## 999 Internal error

This error strongly suggests that there is a bug of SCaml compiler
rather than in your code.

Please do not waste your time to analyze the cause of the error in your code,
but report it at SCaml issues: https://gitlab.com/dailambda/scaml/issues
