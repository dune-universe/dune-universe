SDL2\_mixer bindings for OCaml with Tsdl
----------------------------------------

Tsdl\_mixer provides bindings to
[SDL2_mixer](https://www.libsdl.org/projects/SDL_mixer/) intended to
be used with [Tsdl](http://erratique.ch/software/tsdl).

It has as siblings [tsdl-image](https://github.com/sanette/tsdl-image)
and [tsdl-ttf](https://github.com/sanette/tsdl-ttf).

Note that these bindings are at an early stage and have only been used
minimally.  The interface may change.  Comments and bug reports are
welcome through the [github page](https://github.com/sanette/tsdl-mixer).

## Installation

Via [opam](https://opam.ocaml.org/):

    opam install tsdl-mixer

## Documentation

Documentation is
[here](https://sanette.github.io/tsdl-mixer/Mixer/index.html). It can
be generated with `dune build @doc`, but the binding follows the
SDL2_mixer interface closely, so it may be sufficient to consult
[its documentation](https://www.libsdl.org/projects/SDL_mixer/docs/index.html).
