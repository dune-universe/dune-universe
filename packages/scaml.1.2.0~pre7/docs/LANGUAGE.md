# Writing Smart Contracts

## Learn OCaml

SCaml is a subset of OCaml.  If you do not write OCaml, you will not write SCaml.  Learn OCaml first.

## `open SCaml`

API functions to access Michelson primitives are declared in module `SCaml`.  In SCaml, you should always `open SCaml` first.  (You can skip it but there is almost no point to do it.)

In the normal installation, module `SCaml` should be found at directory `` `opam config var prefix`/lib/scaml ``  Check `SCaml.mli` in this directory or the source code of SCaml to learn what functions are available and their comments.

## Entrypoint

Entry points must be defined in the last linked module, with `[@entry]` attribute.

### Entrypoint typing

The type of the entrypoints must have the form:

```ocaml
ty_parameter -> ty_storage -> operation list * ty_storage
```

where `ty_parameter` and `ty_storage` are the contract's parameter type 
and storage type respectively.  An entrypoint with an incompatible type is rejected by SCaml.

### Multiple entrypoints

SCaml supports the multiple entrypoints introduced in Tezos Babylon.  Put `[@entry]` attribute to each entrypoint definition:

```ocaml
let [@entry] init () _ = Int 0

let [@entry name="do"] do_ () x = x + Int 1
```

Each entry point is named based on the variable name of the definition.  It can be overridden by giving `name` field in the `[@entry]` attribute.  For example, the above definitions introduce 2 entry points, `init` and `do`. 

### Call entrypoints

Use `SCaml.contract'` to access the entrypoint of the given name.  For example, `init` entrypoint of the current contract is obtained by:

```ocaml
Option.get (SCaml.contract' Contract.self "init")
```

## Monomorphism

Michelson is a monomorphic language.  So is SCaml.

If OCaml type-checker, which is used by SCaml, infers polymorphic types for a value, SCaml rejects it.  To avoid it, you have to add type constraints to values whose inferred types are too general.

## No recursion

Michelson does not have an opcode for recursion.  Therefore SCaml does not support recursion either:  `let rec` bindings are rejected.

Still there are still some recursions are available:

* `SCaml` provides mappings and foldings of set, map, and big maps.
* `SCaml` also provides a simple looping: `Loop.left`.

It might be possible to encode recursion in SCaml using Michelson's closure creation and serializers (`Obj.pack` and `Obj.unpack`), but it should be very gas inefficient.  At your own risk.

## Arithmetic types

In SCaml, there are 3 arithmetic types:

`int` 
:    Arbitrary sized integers.  `Int 3`, `Int (-23)`.
     This is not the native `int` type of OCaml but defined in `SCaml`.

`nat`
:    Arbitrary sized natural numbers.  `Nat 0`, `Nat 12345`

`tz`
:    Tezzies.  It takes a float but internally it is handled as a natural number
     of micro tezzies.  `Tz 0.000001` is for 1 mutez.  Note that the size is fixed
	 to 64bits (signed) and `Tz 9223372036854.775807` is the maximum value for `tz`.  Any overflow fails the execution of contracts.

There is no overloading of arithmetic constants.  Even simple integers must be explicitly wrapped with its constructor `Int`.  This is lousy but required for the simplicitly of the language.

Operations over arithmetics are also monomorphic and not overloaded just as OCaml.

* Integers: `+`, `-`, `*`, etc
* Natural numbers: `+^`, `-^`, `*^`, etc.  `^` depicts "positive".
* Tezzies: `+$`, `-$`, `*$`, etc.  `$` depicts "currency".

## Container literals

SCaml has 4 built-in container types: lists, sets, maps, and big maps.
Lists, sets, and maps have literals:

* Lists: `[ Int 1; Int 2; Int 3 ]`
* Sets: `Set [ Nat 1; Nat 2; Nat 3 ]`
* Maps: `Map [ (Nat 1, "1"); (Nat 2, "2"); (Nat 3, "3") ]`

Currently, all the elements in `Set _` and `Map _` must be constants.

## Other crypto related literals

* Bytes: `Bytes "0123456789abcdef"`,  Even number of `[0-9a-f]` characters.
* Address: `Address "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN"`
* Keys: `Key "edpkuSR6ywqsk17myFVRcw2eXhVib2MeLc9D1QkEQb98ctWUBwSJpF"`
* Key hashes: `Key_hash "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"`
* Signatures: `Signature "edsigu4chLHh7rDAUxHyifHYTJyuS8zybSSFQ5eSXydXD7PWtHXrpeS19ds3hA587p5JNjyJcZbLx8QtemuJBEFkLyzjAhTjjta"`
* Timestamps: Timestamp "2019-09-11T08:30:23Z", RFC3339 string.

These constructors must take string literals, therefore SCaml has no conversion 
from `string` to these types. (It is impossible in Michelson.) 

SCaml does not validate the form of strings for now.

## Self

`Contract.self` returns the contract of the code itself.  It has a type `'a contract`
but actually it must agree with the real type of the contract.
      
Unlike Michelson's `SELF` operator, `Contract.self` can appear inside a function.  Even if the function value is sent to another contract, it does not point to the other contract but to the original contract which uses `Contract.self`.

## Contract creation and call

SCaml provides the lowest interface of contract creations and invocations.

### Contract creation

SCaml provides the lowest level of APIs to originate contracts within SCaml:

* `Contract.create_from_tz_code <Michelson code string>` takes a string literal of
   Michelson source code.
* `Contract.create_from_tz_file <Michelson code path name>` takes a string literal of Michelson source file path.  The Michelson code in the source file is included at the compilation time.  Be careful of setting proper build dependnecy if the Michelson source file is generated from another language. 

### Contract call

`Operation.transfer_tokens` is the only API (so far) to call other contracts within SCaml contracts.

### No other inter-contract abstractions

SCaml itself will not provide any highly abstracted easy-to-use framework for contract creation and invocation.

It seems there is no trivial standard way for it.  For example, we can consider OO approach via classes and objects, and functional approach via functors and modules.

We do not want to fix one of possible approaches in SCaml and push it to its users.  Inter-contract frameworks should be built as an SDL with special typing rules which should be compiled down to SCaml.

## Exception

`raise e` can throw an exception `e`. `e` can be predefined or  user-defined exceptions.

Unlike OCaml, exceptions are fatal errors and cannot be caught.  SCaml does not support `try-with`.

Exception values are encoded and used for Michelson opcode `FAILWITH` so that they can be investigated.  

SCaml exception `C` without arguments is encoded to Michelson's string value `C'`.  Exception with arguments, `C args`, is encoded to `Pair C' args'` where:

* `C'` is the string of the path of the exception
* `args'` is the encoding of the arguments

For example:

* `raise Exit` => `"Exit"`
* `raise (Failure "error")` => `Pair "Failure" "error"`
* `raise (MyModule.Error (Int 1, Nat 2))` => `Pair "MyModule.Error" (Pair 1 2)`
