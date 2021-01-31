# Picasso [![Build Status](https://travis-ci.org/ghilesZ/picasso.svg?branch=master)](https://travis-ci.org/ghilesZ/picasso)

Picasso is an Abstract element drawing library which provides several
ways of drawing abstract element.

Visualization of abstract element poses two practical problems:
- abstract elements may be unbounded, are generally defined on more than 2 dimensions, thus rendering them in a 2d context can be difficult.
- managing graphical libraries can be really painful (especially in OCaml) and requires unnecessary efforts.

Picasso solves these issues by providing functions that handle most of
the boilerplate you usually write to draw abstract elements and also
tries to use already installed graphical libraries instead of
requiring heavy dependencies.

### Documentation
you can build it locally by doing ``make doc`` or consult the online [documentation](https://ghilesz.github.io/picasso/picasso/index.html)

## Different Abstract Domains
Picasso handles non-necessarily convex linear spaces of several
dimensions, bounded or not. It provides utilities to draw abstract
element of the [Apron library](https://github.com/antoinemine/apron)
in a straightforward way, plus some other ways of defining drawable
values (see the
[Drawable](https://ghilesz.github.io/picasso/picasso/Picasso/Drawable/index.html)
module).

## Dependencies and build
Picasso can use different graphical library without imposing heavy
dependencies for the end-user. It uses the [opam optional dependency](https://opam.ocaml.org/doc/1.1/Advanced_Usage.html#Installing-packages)
feature, along with [dune's alternative dependencies](https://dune.readthedocs.io/en/stable/concepts.html?highlight=select#alternative-deps), to look for available
graphical library on your system to use those. Also note that using
opam, while installing a new graphical package, picasso will be
recompiled automatically make the new library usable.

## Current backends include:
- interactive [Gtk](http://lablgtk.forge.ocamlcore.org/) window (scrollable, zoomable)
- SVG generation
- LaTex generation (using TikZ)
- 3D .obj file generation (you can use [g3dviewer](http://automagically.de/g3dviewer/), among others, to visualize the 3d model)
- Non-interractive [graphics](https://github.com/ocaml/graphics) window
