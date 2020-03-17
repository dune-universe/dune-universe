# lambda-streams

A lambda-based streaming library. Read the introduction [here][1].


## Installation

For native ocaml:

```
opam install lambda-streams
```

For bucklescript:

```
yarn add lambda-streams
```


## Documentation

See [documentation][2]


## Examples

There are several kinds of streams. The simplest is a `Finite.Sync` stream:

```ocaml
# #require "lambda_streams";;
# open Lambda_streams;;
# let foo = Finite.Sync.from_list [1; 2; 3];;
val foo : int Finite.Sync.input = <fun>
# let bar = foo |> Finite.Sync.map (( * ) 2) |> Finite.Sync.to_list;;
val bar : int list = [2; 4; 6]
```


## License

See [LICENSE][3]


[1]: https://risto-stevcev.github.io/lambda-streams/lambda_streams
[2]: https://risto-stevcev.github.io/lambda-streams
[3]: https://github.com/Risto-Stevcev/lambda-streams/blob/master/LICENSE
