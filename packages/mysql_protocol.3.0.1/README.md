# OCaml mysql_protocol library

[![Build Status](https://travis-ci.org/slegrand45/mysql_protocol.svg?branch=master)](https://travis-ci.org/slegrand45/mysql_protocol)

OCaml implementation of the native MySQL Protocol with the Bitstring library.

## How to install
```
opam install mysql_protocol
```

## Documentation

- [Examples][examples].

[examples]: https://github.com/slegrand45/mysql_protocol/tree/master/examples/

- [Tutorial][tutorial].

[tutorial]: https://github.com/slegrand45/mysql_protocol/blob/master/tutorials/tutorial.pdf?raw=true

- [OCamldoc generated documentation][ocamldoc].

[ocamldoc]: http://slegrand45.github.io/mysql_protocol.site/mysql_protocol/Mysql_protocol/Mp_client/

- Interfaces: [client][client], [data conversion][data], [charsets][charset].

[charset]: https://github.com/slegrand45/mysql_protocol/blob/master/src/mp_charset.mli
[client]: https://github.com/slegrand45/mysql_protocol/blob/master/src/mp_client.mli
[data]: https://github.com/slegrand45/mysql_protocol/blob/master/src/mp_data.mli

## How to build

### Building the project
```
dune build
```

### Building the documentation
```
dune build @doc
```

## Copyright

Copyright (C) 2011-2020, Stephane Legrand.
