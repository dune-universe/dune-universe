# bastet_async

Async implementations for bastet


## Installation

```
opam install bastet_async
```

## Documentation

See [documentation][1]


## Example

```ocaml
# #require "bastet";;
# #require "bastet_async";;

# let foo = Bastet_async.Monad.pure "foo"
  and bar = Bastet_async.Monad.pure "bar"
  and baz = Bastet_async.Monad.pure "baz";;
val foo : string Bastet_async.Monad.t = <abstr>
val bar : string Bastet_async.Monad.t = <abstr>
val baz : string Bastet_async.Monad.t = <abstr>

# let list_of_promises = [foo; bar; baz];;
val list_of_promises : string Bastet_async.Monad.t list =
  [<abstr>; <abstr>; <abstr>]

# Bastet_async.List.Traversable.sequence;;
- : 'a Bastet_async.Monad.t list -> 'a list Bastet_async.Monad.t = <fun>

# Bastet_async.List.Traversable.sequence list_of_promises;;
- : string list Bastet_async.Monad.t = <abstr>
```


## License

See [LICENSE][2]


[1]: http://risto-stevcev.github.io/bastet-async
[2]: https://github.com/Risto-Stevcev/bastet-async/blob/master/LICENSE
