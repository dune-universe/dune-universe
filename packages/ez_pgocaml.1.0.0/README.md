[![Travis-CI Build Status](https://travis-ci.org/OCamlPro/ez_pgocaml.svg?branch=master)](https://travis-ci.org/OCamlPro/ez_pgocaml)

# ez_pgocaml : a simple library to work with pgocaml


## Installation

To build:
```
opam pin .
opam install ez_pgocaml
```

## Usage

A typical usage is to create a database migration tool for your application:

The file `updater.ml` contains:
```
let () =
  EzPGUpdater.main Versions.database Versions.versions

```

where `versions.ml` contains:

```
let database = "testdb"

let update_0_to_1 dbh =
  EzPG.exec dbh
      (Printf.sprintf "CREATE TABLE %s (%s)" 
      "users"
         {|
          user_id bigserial PRIMARY KEY,
          login VARCHAR NOT NULL,
          pwhash bytea NOT NULL
         |})

let update_1_to_2 dbh =
    EzPG.exec dbh
      (Printf.sprintf "CREATE TABLE %s (%s)" 
      "infos"
         {|
          user_id bigserial PRIMARY KEY,
          city VARCHAR
         |})

let versions = [
  0, update_0_to_1;
  1, update_1_to_2;
  ]
```

When ran, the corresponding executable will try to migrate the database
from any version to the latest (2 in this example).
