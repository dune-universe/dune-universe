Interface to FFTW, version 3
============================

[![Build status](https://travis-ci.org/Chris00/fftw-ocaml.png)](https://travis-ci.org/Chris00/fftw-ocaml)

Perquisites
-----------

The FFTW, version 3, with its development files (``fftw3.h``) must be
installed on your system.  For example, on Debian or Ubuntu, you
need to install the package ``libfftw3-dev`` (see the [opam
file](fftw3.opam) for other distributions).


Compilation
-----------

The easier way to install this library is by using [opam][]:

    opam install fftw3

If you cloned this repository, you can use [Dune][dune] to
compile it:

    dune build @install

If your fftw3 header files or libraries are not where you C compiler
expects to find them and `pkg-config` does not return the right
location either, you need to tell the [discover
script](config/discover.ml) where to find them by setting the
environment variable `FFTW3_CFLAGS`:

    FFTW3_CFLAGS="-I/opt/local/include" LDFLAGS="-L/opt/local/lib"

Of course, replace ``/opt/local/include`` (resp. ``/opt/local/lib``) by the
actual paths of your header files (resp. library).

[opam]: https://opam.ocaml.org/
[dune]: https://github.com/ocaml/dune

Usage
-----

Fftw3 contains two submodules differing only by precision ``Fftw3.D`` for
double precision and ``Fftw3.S`` for single precision.  Note that the
functions of the single precision module will raise ``Failure`` if the
corresponding FFTW3 library was not discovered by the configure
script.  See the [mli file](src/fftw3.mli) for more details
or consult the documentation
[online](https://chris00.github.io/fftw-ocaml/doc/fftw3/Fftw3/).


Examples
--------

Some examples need additional libraries:
- [Lacaml](http://mmottl.github.io/lacaml/)
- [Archimedes](https://forge.ocamlcore.org/projects/archimedes/)

If these are not detected by the configure script, the corresponding
examples will not be compiled or will have less features.


Bugs
----

To report bugs, use the Github
[issues page](https://github.com/Chris00/fftw-ocaml/issues).


Development
-----------

If you want to participate to the development of Fftw3, make a
fork of the [project](https://github.com/Chris00/fftw-ocaml) and
submit a pull request.


Licence
-------

FFtw3 is released under the LGPL with the special exception of the
standard library.  See the file LICENSE for more details.

Although not required, it will be appreciated if your product mentions
it uses these bindings.
