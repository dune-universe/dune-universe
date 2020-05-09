[![CircleCI](https://circleci.com/gh/arenadotio/ocaml-mssql.svg?style=shield)](https://circleci.com/gh/arenadotio/ocaml-mssql)
[![Coverage Status](https://coveralls.io/repos/github/arenadotio/ocaml-mssql/badge.svg?branch=master)](https://coveralls.io/github/arenadotio/ocaml-mssql?branch=master)
[![Documentation](https://img.shields.io/badge/documentation-odoc-blue)](https://arenadotio.github.io/ocaml-mssql/mssql/index.html)

**Mssql** is an [Async](https://github.com/janestreet/async) OCaml SQL Server
library, currently using [FreeTDS](https://github.com/kennknowles/ocaml-freetds).

## Features

- Queries run in a background thread and play nicely with Async (assuming
  you're pinning `freetds` so it releases the runtime lock)
- Supports single connections or connection pools
- Supports automatic conversions for common data types
- Supports parameterized queries (although internally it's not very nice;
  we parse the query for $ parameters and then insert quoted params)
- We have workarounds for FreeTDS madness, like how there's no simple way to
  know what date format FreeTDS was configured with
- Intellegently handles concurrent usage for both executing queries and with
  transactions
- Heavily [tested](test/test_mssql.ml)

Regarding concurrent usage, the following code is safe:

```
let%map res1 = Mssql.execute db "SELECT * FROM table_a"
and res2 = Mssql.execute db "SELECT * FROM table_b"
in
...
```

Since we don't support actual concurrency, this will run one query and then
the other (order is not defined).

This is also safe and will never cause a duplicate primary key error, since
we prevent concurrent usage of a connection outside of a transactio if a
transaction is in progress on that connection:

```
Mssql.execute_unit "CREATE TABLE x (id INT PRIMARY KEY)"
>>= fun () ->
let%map () =
  Mssql.with_transaction db (fun db ->
    Mssql.execute_unit db "INSERT INTO x (id) VALUES (1)"
    >>= fun ()->
    Mssql.execute_unit db ~params:Mssql.Param.[Some (Int 1)]
      "DELETE FROM x WHERE id = $1")
and Mssql.execute db "INSERT INTO x (id) VALUES (1)"
```

Obviously this only works if we know about the transaction, so using
`Mssql.begin_transaction` or `Mssql.execute_unit "begin"` won't have this
feature.

## Contributions

This library is heavily optimized for our use-case, but we would love to see
contributions to:

 - Support Lwt and blocking IO
 - Support parameterized queries in a better way
 - Switch to pure OCaml and not use FreeTDS

This is not an exhaustive list -- feel free to create an issue if you're
considering making a new feature and want to know if we'll accept it, or just
open a pull request.

## Installation

You can [find mssql on opam](https://opam.ocaml.org/packages/mssql/):

```
opam install mssql
```

Or you can pin this repo if you want:

```
opam pin add mssql https://github.com/arenadotio/ocaml-mssql.git
```

## Usage

The [tests](test/test_mssql.ml) are full of examples and you can find
[documentation here](https://arenadotio.github.io/ocaml-mssql/mssql/index.html).

### Example

```
Mssql.with_conn ~host ~db ~user ~password (fun db ->
  Mssql.execute_unit db
    "CREATE TABLE example (id INT NOT NULL, value INT NOT NULL)"
  >>= fun () ->
  Mssql.execute_unit db "INSERT INTO example (id, value) VALUES (1, 2)"
  >>= fun () ->
  Mssql.execute db ~params:Mssql.Param.[Some (Int 1)]
    "SELECT id FROM example WHERE id = $1"
  >>| function
  | [ row ] ->
    let id = Mssql.Row.int_exn row "id"
    and value = Mssql.Row.int_exn row "value" in
    printf "Got row with id=%d value=%d" id value
  | _ -> assert false)
```
