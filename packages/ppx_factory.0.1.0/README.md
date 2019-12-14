[![Build Status](https://travis-ci.org/cryptosense/ppx_factory.svg?branch=master)](https://travis-ci.org/cryptosense/ppx_factory)

# ppx_factory

`ppx_factory` is an OCaml preprocessor to derive factory methods and default values from type
definitions.

## Overview

Factories are functions that let you build test data while only specifying the parts that are
relevant to your tests.

`ppx_factory` alows you to derive such functions from type definitions.

Let's take a very basic example. Consider the following type and function:
```ocaml
type person =
  { first_name : string
  ; middle_name : string option
  ; last_name : string
  ; age : int
  ; hobbies : Hobby.t list
  }

let full_name {first_name; last_name; middle_name; _} =
  match middle_name with
  | None -> Printf.sprintf "%s %s" first_name last_name
  | Some m_name -> Printf.sprintf "%s \"%s\" %s" first_name m_name last_name
```

When writing tests for the `full_name` function, you don't want to bother defining `age` and
`hobbies` every time so you use a factory method that looks like:
```ocaml
val person_factory :
  ?first_name: string ->
  ?middle_name: string ->
  ?last_name: string ->
  ?age: int ->
  ?hobbies: Hobby.t list ->
  unit ->
  person
```

and let's you write your test in a more concise manner:
```ocaml
test
  ~input:(person_factory ~first_name:"John" ?middle_name:None ~last_name:"Doe" ())
  ~expected:"John Doe";
test
  ~input:(person_factory ~first_name:"Robyn" ~middle_name:"Rihanna" ~last_name:"Fenty" ())
  ~expected:"Robyn \"Rihanna\" Fenty"
```

By adding the `[@@deriving factory]` attribute to your type definitions, `ppx_factory` will generate
a single factory function for record types or one per constructor for variant types.

You can also use `[@@deriving default]` to generate a single default value per type to use in your
tests.

See the [Detailed usage](https://github.com/cryptosense/ppx_factory#detailed-usage) section for
further information.

## Installation and usage

You can install `ppx_factory` using [opam](https://opam.ocaml.org):
```
$ opam install ppx_factory
```

If you're building your library or app with dune, add the following field to your `library`,
`executable` or `test` stanza:
```
(preprocess (pps ppx_factory))
```
or simply add `ppx_factory` to your `preprocess` field if it's already there.

You can now add the `factory` and/or `default` plugins to `[@@deriving ...]` attributes on type
definitions to derive the corresponding values.

## Detailed usage

### factory

You can use `[@@deriving factory]` to derive factory functions from type definitions both in module
and module signatures eg both in your `.ml` and `.mli` files given that it's an explicit record or
variant type definition.

#### Record types

You can derive factory functions from record type definitions. This will derive a single factory
function that has an optional argument per field. The name of the factory function depends on the
name of the type, `factory` will be derived from type `t` and `<type_name>_factory` for any other
type.

Each optional parameter will expect the same type as the one declared for the corresponding field,
except for option types. A field which has type `a option` will be turned into a optional argument
expecting an `a`.

Because examples are worth a thousand words:
```ocaml
type t =
  { a : int
  ; b : string
  }
[@@deriving factory]

type 'a u =
  { c : 'a list
  ; d : 'a option
  }
[@@deriving factory]
```

will derive the following factory functions:
```ocaml
val factory : ?a: int -> ?b: string -> unit -> t

val u_factory : ?c: 'a list -> ?d: 'a -> unit -> 'a t
```

#### Variant types

You can also derive factory functions from variant type definitions. This will derive one of them
per constructor. Those functions will be named based on the type and the constructor name and have
a `<type_name>_<lowercased_constructor_name>_` prefix.

Constant constructor factories will have a single `unit` argument, while for constructors with tuple
arguments, including 1-element tuples, they will have `?tup<element_tuple_index>` arguments starting
at `?tup0` and for constructors with record arguments they will have optional arguments named as the
corresponding record field.

```ocaml
type t =
  | A
  | B of string
[@@deriving factory]

type 'a u =
  | C of int * 'a option
  | D of
    { some_int : int
    ; some_list : 'a list
    }
[@@deriving factory]
```

will derive the following factory functions:
```ocaml
val a_factory : unit -> t

val b_factory : ?tup0: string -> unit -> t

val u_c_factory : ?tup0: int -> ?tup1: 'a -> 'a u

val u_d_factory : ?some_int: int -> ?some_list: 'a list -> 'a u
```

### default

To derive factory functions, `ppx_factory` relies on the fact that it's able to derive a default
value from a record field or constructor argument's type. For most core types we derive one of our
own whereas for custom types we expect to find one in the right place. Eg for a type `t` we expect a
`default` value to be available in the current scope, for a type `u` we expect `default_u`, for
`A.B.t`, `A.B.default` and so on.

`ppx_factory` also exposes a deriver to spare you the need to write and update those yourself. You
can use it by attaching `[@@deriving default]` to type definitions.

It can be used with most core types, record and variant types. It will derive a single value
which name will be based on the type name, ie `default` for type `t` or `default_<type_name>`
otherwise.

In some cases it's impossible to derive a default from a parametrized type. For example you can't
derive a default value from the following type:
```ocaml
type 'a t = 'a * int
```
because we can't derive a value of type `'a` for any type `'a`.

It will only derive a default value if it can derive a generic one. For instance it's possible to
derive a value that as type `'a option`, `'a list` or `'a array` for any type `'a` (obviously
`None`, `[]` and `[||]`). The `default` deriver will be able to figure that from your explicit type
definitions. If you consider the following two types:
```ocaml
type ('a, 'b) t1 =
  | A of 'a
  | B of 'b

type ('a, 'b) t2 =
  | C of 'a
  | D of 'b
  | E of int
```
`[@@deriving default]` can't derive a default value that as type `('a, 'b) t1` so you will get a
compile error if you try to use it with type `t1`.
On the other hand it can derive a default value that as type `('a, 'b) t2` such as `E 0`.

It is worth noting that **you should never rely on what the actual value of such a default is** as
it is susceptible to change in minor or even patch releases. If your tests depend on a default value
it means you're not using factories right!
