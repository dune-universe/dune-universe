[![Build Status](https://travis-ci.com/cryptosense/ppx_enum.svg?branch=master)](https://travis-ci.com/cryptosense/ppx_enum)

# ppx_enum

`ppx_enum` is an OCaml preprocessor to derive enum-like modules from variant definitions.

## Overview

Enums are bare variants that are intended to represent a flag that can have more values than just true and false.

`ppx_enum` makes it easier to work with enums, in particular handling the conversion to and from strings. This is useful when (de)serializing values (for example, when serializing to store in a database), and cuts down on repetitive boilerplate code.

Consider the following simple example:

```ocaml
type my_enum =
  | Foo
  | Bar
  | Baz
[@@deriving str_enum]
```

The use of `[@@deriving str_enum]` will generate the following functions:

```ocaml
let my_enum_to_string = function
  | Foo -> "Foo"
  | Bar -> "Bar"
  | Baz -> "Baz"

let my_enum_from_string = function
  | "Foo" -> Ok Foo
  | "Bar" -> Ok Bar
  | "Foo" -> Ok Foo
  | _ -> Error ...

let my_enum_from_string_exn = function
  | "Foo" -> Foo
  | "Bar" -> Bar
  | "Foo" -> Foo
  | _ -> invalid_arg ...
```

### Naming of Generated Functions

Generally, the generated functions for type `mytype` will be `mytype_to_string`, `mytype_from_string` and `mytype_from_string_exn`.

The only exception is when using `type t = ...`, in which case `to_string`, `from_string` and `from_string_exn` will be used.

## Installation and Usage

You can install `ppx_enum` using [opam](https://opam.ocaml.org):
```
$ opam install ppx_enum
```

If you're building your library or app with dune, add the following field to your `library`,
`executable` or `test` stanza:
```
(preprocess (pps ppx_enum))
```
or simply add `ppx_enum` to your `preprocess` field if it's already there.

You can now add the `enum` plugin to `[@@deriving ...]` attributes on variant type definitions.

## Customizing the Generated Functions

### Custom Values for Specific Variants

It is possible to customize the string value that will be used to represent a specific variant by using an `[@value]` attribute. An example is worth 1000 words here:

```ocaml
type myenum =
  | Foo [@value "baz"]
  | Bar
[@deriving str_enum]

my_enum_to_string Foo  (* "baz" *)
my_enum_to_string Bar  (* "bar" *)

my_enum_from_string "foo"  (* Error ... *)
my_enum_from_string "bar"  (* Ok Bar *)
my_enum_from_string "baz"  (* Ok Foo *)
```

The attributes will accept any valid suffix of `ppx_enum.str_enum.value`, so the following will work:

```ocaml
type myenum =
  | Foo [@value "foo-1"]
  | Bar [@str_enum.value "bar-1"]
  | Baz [@ppx_enum.str_enum.value "baz-1"]
[@deriving str_enum]
```
