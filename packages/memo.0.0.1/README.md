# Memo [![builds.sr.ht status](https://builds.sr.ht/~zapashcanon/memo.svg)](https://builds.sr.ht/~zapashcanon/memo?)

Memo is an [OCaml] library for [memoïzation].

## Usage

If you had a function `fibo` defined like this:

```ocaml
let rec fibo x =
  if x < 0 then invalid_arg "fibo";
  if x < 2 then x
  else fibo (x - 1) + fibo (x - 2)
```

There's many different ways to memoïze it.

### Simple memoïzation

The easiest one is to rewrite it like this:

```ocaml
let fibo = Memo.memo (fun fibo x ->
  if x < 0 then invalid_arg "fibo";
  if x < 2 then x
  else fibo (x - 1) + fibo (x - 2))
```

It'll use the `Hashtbl` module directly.

I'd like to thank [Sylvain Conchon] who taught me memoïzation and how to write this `memo` function when I was his student.

### Using you own type, `equal` and `hash` functions

We provide a `Make` functor. It can be useful in case you don't want to use polymorphic equality or you are doing things like [hash consing] and you know how to compare or hash your type more efficiently.

```ocaml
let module Mem = Memo.Make(struct
  type t = int
  let equal = (=)
  let hash = Hashtbl.hash
end)

let fibo = Mem.memo (fun fibo x ->
  if x < 0 then invalid_arg "fibo";
  if x < 2 then x
  else fibo (x - 1) + fibo (x - 2))
```

### Forgetful memoïzation

We provide a `MakeWeak` functor. It works like the previous one, but the bindings in the memoïzation cache will be weak, allowing the garbage collector to remove them if they are not used somewhere else.

```ocaml
let module Mem = Memo.MakeWeak(struct
  type t = int
  let equal = (=)
  let hash = Hashtbl.hash
end)

let fibo = Mem.memo (fun fibo x ->
  if x < 0 then invalid_arg "fibo";
  if x < 2 then x
  else fibo (x - 1) + fibo (x - 2))
```

I'd like to thank [Jean-Christophe Filliâtre] who taugh me forgetful memoïzation when I was doing research on [binary decision diagram] under his direction while I was a first year master student.

### Fake memoïzation

We provide a `Fake` functor. It is useful if you want to quickly test a function you memoïzed with our `Make` or `MakeWeak` functor, but without memoïzing it. It'll basically do nothing and should be equivalent to your initial non-memoïzed function.

```ocaml
let module Mem = Memo.Fake(struct
  type t = int
  let equal = (=)
  let hash = Hashtbl.hash
end)

let fibo = Mem.memo (fun fibo x ->
  if x < 0 then invalid_arg "fibo";
  if x < 2 then x
  else fibo (x - 1) + fibo (x - 2))
```

### Using your own defined cache

With the `Mk` functor, you can also directly provide a `Cache` module, which should have the signature `Hashtbl.S`. We will include your cache module and use it to define a `memo` function:

```ocaml
let module Mem = Memo.Mk(
  Hashtbl.Make(struct
    type t = int
    let equal = (=)
    let hash = Hashtbl.hash
  end)
end)

let fibo = Mem.memo (fun fibo x ->
  if x < 0 then invalid_arg "fibo";
  if x < 2 then x
  else fibo (x - 1) + fibo (x - 2))
```

This example is useless and equivalent to using the `Make` functor directly.

If you find a real use case for this which doesn't need new dependencies, contact me and I'll be happy to add a new functor to the library.

It should be useful only if you want to use another `Hashtbl` implementation or things like this.


### Tuning

There's a default value for the initial cache size. You can set it to the value of your choice, reset it to the default and get the current value like this:

```ocaml
Memo.set_initial_cache_size 1024;
Memo.reset_initial_cache_size ();
let curr_size = Memo.get_initial_cache_size ()
```

Note that with the current implementation of hash tables in OCaml, it's better if you choose a power of two. You may saw some code using a prime number, it's because some years ago it was the best thing to do as the hash tables implementation was different. [Jean-Christophe Filliâtre] explained this to me, thanks again ! Also keep in mind that if you use your own defined cache using the `Mk` functor, it may not be the right thing to do.

## Build

To build the library:

```sh
scripts/build.sh
```

To clean after building:

```sh
scripts/clean.sh
```

## Documentation

To build the documentation:

```sh
scripts/doc.sh
```

You can open it in your browser using:

```sh
scripts/view_doc.sh
```

## Tests

To run the tests:

```sh
scripts/test.sh
```

To run the tests with coverage report:

```sh
scripts/coverage.sh
```

You can open the tests coverage report in your web browser using:

```sh
scripts/view_coverage.sh
```

## Format

You can format the code using:

```sh
scripts/format.sh
```

## License

See [LICENSE].

## Changelog

See [CHANGELOG].

[CHANGELOG]: ./CHANGELOG.md
[LICENSE]: ./LICENSE.md

[binary decision diagram]: https://en.wikipedia.org/wiki/Binary_decision_diagram
[Jean-Christophe Filliâtre]: https://www.lri.fr/~filliatr/
[hash consing]: https://en.wikipedia.org/wiki/Hash_consing
[memoïzation]: https://en.wikipedia.org/wiki/Memoization
[OCaml]: https://en.wikipedia.org/wiki/OCaml
[Sylvain Conchon]: https://www.lri.fr/~conchon/
