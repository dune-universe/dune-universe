OCaml-gettext - Internationalization library for OCaml (i18n)
=============================================================

[![Travis status][travis-img]][travis]
[![AppVeyor status][appveyor-img]][appveyor]

Internationalization of a program means that the program have the possibility
to handle different language. It can output messages which depend on the
language of the user.  Typically, if a program can output "bonjour" for a
french user, and "hello" for an english user, this program is
internationalized.

GNU gettext is one of the standard solutions for i18n. You just need to use
special functions to translate strings. These functions are used when the
program is running to do the translation and when compiling the program to
extract the strings automatically. In ocaml-gettext these functions are "s_",
"f_","sn_" and "fn_". They are both used to translate at runtime and to extract
strings for translation.

ocaml-gettext provides enough service to build a internationalized program. It
comes with :

* a pure Ocaml implementation, based on Camomile,
* an alternative implementation with a binding to GNU gettext library,
* `ocaml-gettext` a tool to extract strings from Ocaml source.

[travis]:         https://travis-ci.org/gildor478/ocaml-gettext
[travis-img]:     https://travis-ci.org/gildor478/ocaml-gettext.svg?branch=master
[appveyor]:       https://ci.appveyor.com/project/gildor478/ocaml-gettext
[appveyor-img]:   https://ci.appveyor.com/api/projects/status/4dalakr6ixnhotve/branch/master?svg=true

Installation
------------

The recommended way to install ocaml-gettext is via the [opam package manager][opam]:

```sh
$ opam install gettext gettext-camomile gettext-stub
```

Documentation
-------------

* API documentation is
  [available online](https://gildor478.github.io/ocaml-gettext).
* [Reference manual](doc/reference-manual.md)

Examples
--------

* A [library](examples/library)
* A [program](examples/program)
* A [GUI](examples/gui)
