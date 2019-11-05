# shared-secret
Abstract (encapsulated) messages or hidden (semi-deterministic) exceptions using OCaml's module system.

[![Build Status](https://travis-ci.org/marcoonroad/shared-secret.svg?branch=master)](https://travis-ci.org/marcoonroad/shared-secret)

A package inspired by this post: https://existentialtype.wordpress.com/2012/12/03/exceptions-are-shared-secrets/


## Installation

If available on OPAM:

```
$ opam install shared-secret
```

Otherwise, through this project directory (changes should be committed):

```
$ opam install .
```

The library is linked by Dune/Findlib as `shared-secret` as well.


## API for version 0.1

  This package actually provides only one module file called `Shared_secret`. This module contains 2 functors,
which are needed for different purposes. They are `Message` and `Exception`, parameterized by:

```ocaml
sig type t end
```

but the Message functor is generative, so it also takes `( )`.

  The first is to encode and decode existentially typed values. The use becomes obvious when we declare an
exception of an existential type. For instance:

```ocaml
module StringMessage = Message (String) ( );;

exception AbstractString of StringMessage.t;;
```

  In the above example, StringMessage will contain 2 submodules with these signatures:

```ocaml
module Encoder : sig
  val encode : String.t -> StringMessage.t
end;;

module Decoder : sig
  val decode : StringMessage.t -> String.t
end;;
```

  So the `AbstractString` exception can be caught, but not decoded without `Decoder.decode`:

```ocaml
let open StringMessage in
try raise (AbstractString (Encoder.encode "Hello, OCaml!")) with
| AbstractString msg -> Decoder.decode msg;; (* = "Hello, OCaml!" *)
```

  This encoding/decoding process imposes no additional runtime costs, internally it is just
the identity function.

  On other hand, the Exception functor is used when we don't like to expose the exception itself,
thus is impossible to catch it by explicit binding (pattern matching), it can only be intercepted,
but not revealed without the proper handler.

```ocaml
module StringException = Exception (String);;

let unsafe ( ) = StringException.Raiser.raise "Hello, World!";;

try unsafe ( ) with
| _ -> ( );; (* // discards the exception, don't use it on production code *)

let open StringException in
Handler.handle unsafe (fun str -> str);; (* = "Hello, World!" *)
```

  By the above example, we can deduce the following signature of the submodules:

```ocaml
module Raiser : sig
  val raise : String.t -> 'a
end;;

module Handler : sig
  val handle : (unit -> 'a) -> (String.t -> 'a) -> 'a
end;;
```


## API for version 0.2

There is no break of compatibility with the previous API. This current
API provides 2 additional modules in the namespace of `Shared_secret`,
called `Token` and `Box`. They are inspired on the Sealing Pairs
mechanisms of the [Object Capability Model](http://http://erights.org/elib/capability/ode/ode-capabilities.html). The historical origins of
these pairs can be found at [Morris73](#morris-73), though.

The `Token.t` value is an unforgeable and revocable reference. This
unique identifier is used by the `Box` module to seal any `'value` as
`'value Box.t`. Despite the content type being known (different of the
module types for version 0.1, this type is parametric rather than abstract),
the sealed value can only be known by the proper `Token.t` which
sealed it beforehand.

The following module interfaces can help you how to understand their use:

```ocaml
module type IToken = sig
  type t
  type revoker

  exception AlreadyRevoked
  exception RevokedToken

  val create  : unit -> t * revoker
  val revoke  : revoker -> unit
  val revoked : t -> bool
  val (=)     : t -> t -> bool
end;;

module Token : IToken;;

module type IBox = sig
  type 'value t

  exception InvalidToken

  module Sealer   : sig val seal   : Token.t -> 'value   -> 'value t end;;
  module Unsealer : sig val unseal : Token.t -> 'value t -> 'value   end;;
end;;

module Box : IBox;;
```

The list below also shows where exceptions may be thrown:

* `Token.AlreadyRevoked`, prone to occur on `Token.revoke` if the `Token.t` was already revoked previously
* `Token.RevokedToken`, occurs on either `Box.Sealer.seal` or `Box.Unsealer.unseal` if `Token.revoke` was called at least once
* `Box.InvalidToken`, always raised on `Box.Unsealer.unseal` if the `'value Box.t` was sealed with a different `Token.t`


## API for version 0.3

In this version 2 new things were added. One thing is a module called `Pair` while the other is a generative functor called
`Revocable`. This current API provides
two functions in the `Pair` module, which are the `Exception` and `Box` operations being projected, somehow. Value restriction
appears on both functions, though. The first function makes a pair of exception "handlers", the first element is the "raiser"
while the second is the respective "catcher". The second function, on the other hand, generates a associated pair of projection
and injection for a generic `Box` type. So, the needed `Token` value is hidden and shared among these operations. The whole purpose
of this module is to provide shortcuts/aliases for other modules. Fair enough of
cheap talking, the entire signature of `Pair` is:

```ocaml
module Pair : sig
  val exceptional : unit -> ('value -> 'failure) * ((unit -> 'result) -> ('value -> 'result) -> 'result)
  val sealing     : unit -> ('a -> 'a Box.t) * ('a Box.t -> 'a)
end;;
```

The semantics of raised exceptions from the module level counterparts are still preserved and untouched.

'Cause the `Token` value is hidden on `sealing` result operations, we need some way to revoke these operations
without even touch the token itself. Here comes the generative `Revocable` functor. Being a generative functor, it will
take an `( )` as argument, and so, yield a fresh module. This fresh module revokes generic lambda proxies, while keeping
the original lambda expressions intact. That said, this module must be equipped with a `revoke` operation, once this operation
is called, the fresh module itself becomes useless together with its respective generated lambda proxies. On any attempt to use
these lambda proxies once the revocation was triggered, an exception `RevokedReference` (from the fresh module) is raised. When
we call `revoke` again, another exception called `AlreadyRevoked` (also from the fresh module) is also raised. The signature below
describes with some detail this generative functor:

```ocaml
module Revocable : functor ( ) -> sig
  exception RevokedReference
  exception AlreadyRevoked

  val revocable : ('a -> 'b) -> ('a -> 'b)
  val revoke    : unit -> unit
end;;
```


## Contributors

- [zapashcanon](https://github.com/zapashcanon)


## References

* <a name="morris-73"> </a> [ Morris73 ] Protection in Programming Languages, 1973 - James H. Morris Jr.
