[![Build status](https://travis-ci.org/kennknowles/ocaml-freetds.png)](https://travis-ci.org/kennknowles/ocaml-freetds)

OCaml FreeTDS Binding
=====================

https://github.com/kennknowles/ocaml-freetds

An OCaml binding to the `ct-lib` portion of the `freetds` library, for interfacing with Sybase and Microsoft SQL databases.


Feature Summary
---------------

 - Direct binding to the `ct-lib` interface
 - Supports `Dbi_freetds` module included with `ocamldbi` 

See the [online
documentation](https://kennknowles.github.io/ocaml-freetds/doc).


Known Bugs And Limitations
--------------------------

 - Some data types, such as datetimes, are returned as strings,
   because I haven't had time to write a good binding for them yet.

 - It would be nice to bind the dblib interface as well, and maybe
   even have the DBI module be able to use either, because I understand
   they have slightly different features


Installation
------------

The easier way is to use opam:

    opam install freetds

Quick Version:

```
$ tar xjvf ocaml-freetds-<version>.tar.bz2
$ cd ocaml-freetds-<version>
$ make
$ make install
```

Long Version:

1)	`make`
	This will build ocaml-freetds.  This requires FreeTDS development
	packages to be installed, see the `depexts` section in
	[freetds.opam](freetds.opam).

2)	`make install`
	You may need to run this as root, or someone with permissions to the findlib's destdir.
	This should install ocaml-freetds as a findlib package, so you can use
	'ocamlfind' to build things with it.

## Special OSX Instructions

This seems to be the easiest way to get ready on OSX:

```
brew install ocaml
brew install opam
opam install jbuilder
```


Examples
--------

In the examples subdirectory is a simple SQL dispatcher script written against
the `Ct` module, and also one for the `Dbi_freetds` module.


Contributors
------------

- [Kenn Knowles](https://github.com/kennknowles) ([@KennKnowles](http://twitter.com/KennKnowles))
- [Christophe Troestler](https://github.com/Chris00)
- [Brendan Long](https://github.com/brendanlong)

License
-------

ocaml-freetds is distributed under the terms of the GNU Lesser
Public License, version 2.1 See the file COPYING.LIB for details

