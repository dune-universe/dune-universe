SDL2\_ttf bindings for OCaml with Tsdl
--------------------------------------

*WARNING*: These bindings are subject to change.

Tsdl\_ttf provides bindings to
[SDL2_ttf](https://www.libsdl.org/projects/SDL_ttf/) intended to
be used with [Tsdl](http://erratique.ch/software/tsdl).

It has as siblings [tsdl-image](https://github.com/sanette/tsdl-image)
and [tsdl-mixer](https://github.com/sanette/tsdl-mixer).

Note that these bindings are at an early stage and have only been used
minimally.  The interface may change.  Comments and bug reports are
welcome through the
[github page](https://github.com/sanette/tsdl-ttf).

## Installation

Via [opam](https://opam.ocaml.org/):

    opam install tsdl-ttf

## Documentation

Documentation is
[here](https://sanette.github.io/tsdl-ttf/Ttf/index.html), it can be
generated with `dune build @doc`, (or `./make_doc.sh`) but the binding
follows the SDL2_ttf interface closely, so it may be sufficient to
consult
[its documentation](https://www.libsdl.org/projects/SDL_ttf/docs/index.html).

## WARNING V0.3 Breaking change

Starting from 0.3, the library name is the same as the opam package
name `tsdl-ttf`. (The library name used to be `tsdl_ttf`, which
was confusing).
