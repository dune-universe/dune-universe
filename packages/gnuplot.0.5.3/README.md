Gnuplot-OCaml - Simple interface to Gnuplot
===========================================

---------------------------------------------------------------------------

Gnuplot-OCaml provides a simple interface to [Gnuplot](http://www.gnuplot.info)
from [OCaml](http://www.ocaml.org).  The API supports only 2D graphs and was
inspired by [FnuPlot](https://github.com/fsprojects/FnuPlot).

Installation
------------

From [OPAM](http://opam.ocaml.org)

    $ opam install gnuplot

From Source

    $ make
    $ make install

__NOTE__: For a persistent X11 terminal add  `set term x11 persist` to your
`.gnuplot` file in your home directory.

Usage
-----

### Documentation

The API-documentation of this distribution can be built with `make doc`.
It can also be found [online](http://ogu.bitbucket.io/gnuplot-ocaml/api/).

### Examples

This simple example

    :::ocaml
    open Gnuplot

    let () =
      let gp = Gp.create () in
      Gp.plot_many gp ~range:(Range.XY (-10., 10., -1.5, 1.5))
       [ Series.lines_func  "sin(x)" ~title:"Plot a line" ~color:`Blue
       ; Series.points_func "cos(x)" ~title:"Plot points" ~color:`Green ];
      Gp.close gp

generates the following plot:

![Simple Plot](http://ogu.bitbucket.io/simple_plot.png)

For more examples please refer to the `examples`-directory of this
distribution.  You can build the examples with jbuilder, e.g.

    $ jbuilder gbm_paths.exe

Running

    $ _build/default/gbm_paths.exe

displays 10 simulated paths of geometric Brownian motion:

![GBM Paths](http://ogu.bitbucket.io/gbm_paths.png)


Contact Information
-------------------

In case of bugs, feature requests and similar, please contact:

  * Oliver Gu <gu.oliver@yahoo.com>
