# ppx-open
A ppx rewriter that provides idiomatic selective `open`s in OCaml.

## Syntax
`ppx_open` adds selective `open` constructs, wrapped inside `[%%open: {|...|}]`,
that may expose
- modules
- module types
- values
- types (and their constructors)

of a given module. 

For instance, the following code:
```ocaml
(* Foo.ml *)

let val_1 = 1

type type_1 = A | B | C
type 'a type_2 = D of 'a

module Mod_1 = struct 
  let val_2 = 3
end

(* Bar.ml *)
[%%open: {| 
    Foo.( val_1 as renamed
        , type type_1
        , type type_2 (..) as renamed_type_2
        , module Mod_1) |}]
```
is rewritten into: 
```ocaml
open (struct
  let renamed = Foo.val_1
  type type1 = Foo.type_1
  type 'a renamed_type_2 = 'a Foo.type_2 = D of 'a
  module Mod_1 = Foo.Mod_1
end)
```

Optionally, one may prefer to use the syntax: `{%%open| ... |}` which is syntactic equivalent to `[%%open: {| ... |}]`. 

### Modules and Module Types

The grammar for modules and module types is defined by 
```
module_item ::= module module_ident [as module_ident]
module_type_item ::= module type module_ident [as module_ident]
module_ident ::= <upper identifier>
```

The above grammar is designed to mimic the syntax of OCaml (and hence should be relatively intuitive). Modules and module types are specified by an "upper identifer", an identifier beginning with a uppercase letter e.g. `Foo`, `Bar`, `S_with_syntax`, etc. 


`ppx-open` implements the opening of modules and module types via module (and module type) aliases. The optional `as module_ident` allows for local aliases. 
For example: 
```ocaml
(* monad.ml *)

module type S = sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end


module Option : S with type 'a t = 'a option = struct
  type 'a t = 'a option = Some of 'a | None
  
  let return x = Some x
  let bind m ~f = 
    match m with
    | Some x -> f x
    | None -> None
end

(* Bar.ml *)
[%%open: {| Monad.(module Option as Option_monad, module type S as Monad) |}]
```
is rewritten to:
```ocaml
open (struct
  module Option_monad = Monad.Option
  module type Monad = Monad.S
end)
```


### Values

The opening of values is similar to [Modules and Module Types](#module-and-module-types). They have the following grammar:
```
value_item ::= value_ident [as value_ident]
value_ident ::= <lower identifier>
```




Let value bindings are used to alias values, for example: 
```ocaml
(* Foo.ml *)
let print ?(suffix=" world") prefix = print_endline (prefix ^ suffix)

(* Bar.ml *)
[%%open: {| Foo.(print as foo_print) |}]
```
is rewritten to: 
```ocaml
open (struct
  let foo_print = Foo.print
end)
```

### Types

<p align="center">🚧&nbsp;&nbsp;&nbsp;<b><i>Warning: Relies on compiler internals</i></b>&nbsp;&nbsp;&nbsp;🚧</p>

There are 2 kinds of type items:
- closed: `type t`
- open: `type t (..)`


An opened type item results in opening a type and exposing the type definition locally. Whereas a closed type item results a type alias. 

This results in the following grammar for type items:
```
type_item ::= type type_ident type_kind [as type_ident]
type_kind ::= (..) | epsilon 
```


For example: 
```ocaml
(* Foo.ml *)

type 'a type_1 = A | B | C | D of int | E of 'a

type ('a, 'b) type_2 = Pair of ('a * 'b)

(* Bar.ml *)
[%%open: {| Foo.(type type_1, type type_2 (..) as t) |}]
```
is rewritten to:
```ocaml
open (struct
  (* A type alias *)
  type 'a type_1 = 'a Foo.type_1
  (* Exposes type definition (constructors in this case) *)
  type ('a, 'b) t = ('a, 'b) Foo.type_2 = Pair of ('a * 'b)
end)
```

Both expansions rely on typing information from the environment (obtained using internal compiler libraries) to generate the correct typing manifest 
(and obtain the type declaration for the type definition). 


