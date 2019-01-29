[![Build status](https://travis-ci.org/kennknowles/ocaml-freetds.png)](https://travis-ci.org/kennknowles/ocaml-freetds)
[![Build status](https://ci.appveyor.com/api/projects/status/7mk4ksdqea78gqoh/branch/master?svg=true)](https://ci.appveyor.com/project/kennknowles/ocaml-freetds/branch/master)

OCaml FreeTDS Binding
=====================

[OCaml FreeTDS](https://github.com/kennknowles/ocaml-freetds) is a
binding to portions of the `db-lib` and `ct-lib` of the
[FreeTDS](http://www.freetds.org/) library, for interfacing with
Sybase and Microsoft SQL databases.

See the [online
documentation](https://kennknowles.github.io/ocaml-freetds/doc).

Installation
------------

Before installing the bindings, you must have the development package
of FreeTDS on your system.  See [freetds.opam][] to know what (system)
package to install.

The simplest way of installing this library is to use
[opam](https://opam.ocaml.org/):

    opam install freetds
	
If you cloned this repository, issue `make` to compile.  To install:
`make install`.

To run the tests, you must first set the environment variables
`MSSQL_TEST_USER`, `MSSQL_TEST_PASSWORD`, `MSSQL_TEST_SERVER` and
`MSSQL_TEST_DATABASE` and, if necessary, `TDSVER` (see
[appveyor.yml][] for example values).  Then type `make test`.

[freetds.opam]: freetds.opam
[appveyor.yml]: appveyor.yml

Examples
--------

In the examples subdirectory is a simple SQL dispatcher script written
against the `Dblib` and the `Ct` modules.


Known Bugs And Limitations
--------------------------

- Some data types, such as datetimes, are returned as strings,
  because I haven't had time to write a good binding for them yet.


Contributors
------------

- [Kenn Knowles](https://github.com/kennknowles)
  ([@KennKnowles](http://twitter.com/KennKnowles))
- [Christophe Troestler](https://github.com/Chris00)
- [Brendan Long](https://github.com/brendanlong)

License
-------

ocaml-freetds is distributed under the terms of the GNU Lesser
Public License, version 2.1 See the file [LICENSE.md](LICENSE.md)
for details.

