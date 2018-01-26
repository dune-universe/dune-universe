# A safe but strange way of modifying OCaml compiler

## SML style overloading

SML style overloading is very simple way to overload things. 
Much simpler than Haskell type classes, so you cannot derive overloading from overloaded values.
But it is still very useful.

## Getting started with Ppx_overloading

First, we need a seed of an overloaded value, with a polymorphic type, but without any actual definition: 

```ocaml
module Loaded = struct
  val%overloaded (+) : 'a -> 'a -> 'a (* Yes you can write [val] in structures *)
end
```

Here we declare `Loaded.(+)` to be an overloaded polymorphic function of type `'a -> 'a -> 'a`.
In this `Loaded` module, we stack sub-modules which provide overloaded instances for this `(+)`:

```ocaml
module Loaded = struct
  val%overloaded (+) : 'a -> 'a -> 'a
  module Int = struct
    let (+) = Pervasives.(+)
  end
  module Float = struct
    let (+) = Pervasives.(+.)
  end
end
```

Here we have `plus`es for `int` and `float`. Now the preparation is done!
Let's use `Loaded.(+)` as if it is overloaded by these two instances!:

```ocaml
open Loaded
let _ = 
  assert (1 + 2 = 3);
  assert (1.2 + 3.4 = 4.6) (* See it is not [+.] but [+] !!! *)
```

The overload resolution of `Loaded.(+)` is done by `ppx_overload`.
If `ppx_overload` finds a use of a value whose declaration is with `[%overloaded]`, 
`ppx_overload` tries to replace it by one of the instances match with the current typing context.

### Complete example

Let's see how it works with `ppx_overload`.  First, prepare a source file `test.ml` with 
the following contents:

```ocaml
module Loaded = struct
  val%overloaded (+) : 'a -> 'a -> 'a
  module Int = struct
    let (+) = Pervasives.(+)
  end
  module Float = struct
    let (+) = Pervasives.(+.)
  end
end

open Loaded
let _ = 
  assert (1 + 2 = 3);
  assert (1.2 + 3.4 = 4.6); (* See it is not +. but + !!! *)
  prerr_endline "OK!"
```

Compile it with `ppx_overload`:

```shell
$ ocamlc -ppx <directory installs ppx_overload>/ppx_overload test.ml
$ ./a.out
OK!
```

You can check how the preprocessor resolves by `ppx_overload -debug`:

```shell
$ <directory installs ppx_overload>/ppx_overload -debug test.ml
module Loaded =
  struct
    external (+) : 'a -> 'a -> 'a = "%OVERLOADED"
    module Int = struct let (+) = Pervasives.(+)  end
    module Float = struct let (+) = Pervasives.(+.)  end
  end
open Loaded
let _ =
  assert ((Loaded.Int.(+) 1 2) = 3);
  assert ((Loaded.Float.(+) 1.2 3.4) = 4.6);
  prerr_endline "OK!" 
```

You can see the uses of `Loaded.(+)` are replaced 
by either `Loaded.Int.(+)` or `Loaded.Float.(+)` 
which match with the typing contexts appropriately.

The declaration of the overloaded `(+)` is replaced by one of an external value of primitive `"%OVERLOADED"`.
Actually this primitive is a dummy and it does not exit.  It introduces a dummy value 
for the targets of `ppx_overload` overload resolution.

## Special syntax

To declare an overloaded value in a structure (`.ml`), use
 
```ocaml
val%overloaded name : type
```

or 

```ocaml
external name : type = "%OVERLOADED"
(* %OVERLOADED is used instead of %overloaded due to a historical reason *)
```

Actually the former is a `ppx_overload`'s syntax sugar of the latter.

The same `val%overloaded name : type` can be used in signatures (.mli) too:

```ocaml
module X : sig
  val%overloaded (+) : 'a -> 'a -> 'a
end = struct
  val%overloaded (+) : 'a -> 'a -> 'a
end
```
