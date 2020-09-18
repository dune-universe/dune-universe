# Picasso
Picasso is an Abstract element drawing library which provides several
ways of drawing abstract element. As abstract elements may be
unbounded, and are generally defined on more than 2 dimensions,
rendering them in a 2d context can be difficult.  Picasso handles most
of the boilerplate you usually write to draw abstract elements and
allows you to view those with minimum effort from your end.

### Documentation
you can build it locally by doing ``make doc`` or consult the online [documentation](https://ghilesz.github.io/picasso/picasso/index.html)

## Different Abstract Domains
Picasso handles non-necessarily convex linear spaces of several
dimensions, bounded or not. It provides utilities to draw abstract
element of the [Apron library](https://github.com/antoinemine/apron)
in a straightforward way, plus some other ways of defining drawable
values (see the [Drawable](https://ghilesz.github.io/picasso/picasso/Picasso/Drawable/index.html)module).

## Different backends
Picasso features different ways of visualizing abstract elements:
- interactive [Gtk](http://lablgtk.forge.ocamlcore.org/) window (scrollable, zoomable) 
- LaTex generation
- 3D .obj file generation (you can use [g3dviewer](http://automagically.de/g3dviewer/), among others, to visualize the 3d model)
<!-- - Non-interractive [graphics](https://github.com/ocaml/graphics) window (todo) -->
