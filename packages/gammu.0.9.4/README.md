Gammu
=====

This is a binding from OCaml to [libGammu](http://wammu.eu/), which
provides an abstraction layer to work with different cell phones from
different vendors.

Install
-------

First, you need to install the gammu development files; see
[gammu.opam](gammu.opam) to get the right package for your system.
The build script will use `pkg-config` to determine the right flags
for your system.  If `pkg-config` does not work (e.g. you are on
Windows), set the environment variable `OCAML_GAMMU_CFLAGS` to the
compiler flags to find the header files (e.g. `-I/usr/include/gammu`)
and `OCAML_GAMMU_LIBS` to the linking flags (e.g. `-lgammu -lm`).
Beware that, if these variables are set, they take precedence over the
pkg-config discovery.

The easier way to install this library is to use [opam][]:

    opam install gammu

If you prefer to compile by hand, issue `jbuilder build
@install` in the directory in which you downloaded this code.

To compile the library with debugging output turned on for the C
stubs, define the environment variable `OCAML_GAMMU_DEBUG`.

[opam]: https://opam.ocaml.org/

Documentation
-------------

The documentation is available in [gammu.mli](src/gammu.mli) or
[online](https://Chris00.github.io/ocaml-gammu/doc).
