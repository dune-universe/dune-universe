*JsOfOCairo* is an OCaml (4.02.2+) library to reuse Cairo-based drawing code in web browsers.
It's an adapter, implementing (a reasonable subset of) the interface of `Cairo OCaml <https://github.com/Chris00/ocaml-cairo/>`_
targeting HTML5 canvas elements as exposed to OCaml by `js_of_ocaml <https://ocsigen.org/js_of_ocaml/>`_ (2.8+).

It's licensed under the `MIT license <http://choosealicense.com/licenses/mit/>`_.
It's available on `OPAM <https://opam.ocaml.org/packages/JsOfOCairo/>`_.
Its `source code <https://github.com/jacquev6/JsOfOCairo>`_ is on GitHub.

Here is `DrawGrammar <https://jacquev6.github.io/DrawGrammar/>`_, a real-life aplication of JsOfOCairo.

There is no real documentation besides this README.rst file.
If a function is present in JsOfOCairo, it should behave as described in the `Cairo OCaml Tutorial <http://cairo.forge.ocamlcore.org/tutorial/index.html>`__.
Have a look at the `interface file <https://github.com/jacquev6/JsOfOCairo/blob/master/src/S.ml>`_ to know which functions *are* implemented.
You can also have a look at the `test file <https://github.com/jacquev6/JsOfOCairo/blob/master/src/DrawingTests.ml>`_ to see if what you're looking for is implemented and tested.

Questions? Remarks? Bugs? Want to contribute? `Open an issue <https://github.com/jacquev6/JsOfOCairo/issues>`__!

.. image:: https://img.shields.io/travis/jacquev6/JsOfOCairo/master.svg
    :target: https://travis-ci.org/jacquev6/JsOfOCairo

.. image:: https://img.shields.io/github/issues/jacquev6/JsOfOCairo.svg
    :target: https://github.com/jacquev6/JsOfOCairo/issues

.. image:: https://img.shields.io/github/forks/jacquev6/JsOfOCairo.svg
    :target: https://github.com/jacquev6/JsOfOCairo/network

.. image:: https://img.shields.io/github/stars/jacquev6/JsOfOCairo.svg
    :target: https://github.com/jacquev6/JsOfOCairo/stargazers

Quick start
===========

Install from OPAM::

    $ opam install JsOfOCairo

The files described below are available as a `demo directory <https://github.com/jacquev6/JsOfOCairo/tree/master/demo>`_.
Have a look at this directory for the details about compiling.
In particular see the `jbuild file <https://github.com/jacquev6/JsOfOCairo/blob/master/demo/jbuild>`_
and the `call to jbuilder <https://github.com/jacquev6/JsOfOCairo/blob/master/demo/demo.sh>`_.

Create a functor implementing your drawing code against the ``JsOfOCairo.S`` signature.
File ``drawings.ml``::

    module Make(C: JsOfOCairo.S) = struct
      let draw ctx =
        C.arc ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:5.;
        C.stroke ctx
    end

Instantiate this functor with ``Cairo`` to create a command-line program.
File ``draw_on_command_line.ml``::

    module Drawings = Drawings.Make(Cairo)

    let () = begin
      let image = Cairo.Image.create Cairo.Image.ARGB32 ~width:100 ~height:100 in
      Drawings.draw (Cairo.create image);
      Cairo.PNG.write image "draw_on_command_line.png";
    end

Instantiate the same functor with ``JsOfOCairo`` and compile it using js_of_ocaml to create a Javascript file.
File ``draw_in_browser.ml``::

    module Drawings = Drawings.Make(JsOfOCairo)

    let () = Js.export "draw" (fun canvas ->
      Drawings.draw (JsOfOCairo.create canvas)
    )

And call this javascript file in an HTML document.
File ``draw_in_browser.html``::

    <!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="utf-8">
      <meta http-equiv="Content-Type" content="text/html; charset=utf-8">

      <title>JsOfOCairo demo</title>
    </head>
    <body>
      <h1>PNG image from command-line</h1>
      <img src="draw_on_command_line.png" />
      <h1>HTML5 canvas</h1>
      <canvas id="drawings" width="100" height="100"></canvas>
      <script src="_build/default/draw_in_browser.bc.js"></script>
      <script>
        draw(document.getElementById("drawings"));
      </script>
    </body>
    </html>

What is **not** implemented
===========================

Contributions in this area are welcome.
Please `start a discussion <https://github.com/jacquev6/JsOfOCairo/issues>`_ before doing anything to avoid wasting time.

Only a basic subset of functions related to text has been implemented.
The HTML5 canvas interface doesn't expose much, so this library contains approximations of some Cairo functions like ``get_text_extents``.

Everything involving a ``Surface.t`` has been dismissed.
This doesn't make much sense in an HTML5 context.
An attempt has been made to implement ``set_source_for_image`` using a hidden canvas but it's been unsuccessful.

A few other functions commented out at the beginning of
`JsOfOCairo_S.mli <https://github.com/jacquev6/JsOfOCairo/blob/master/src/JsOfOCairo_S.mli>`_ have been dismissed as well.
