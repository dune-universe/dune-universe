# ocp-reveal

OCaml bindings to [reveal.js] library.

`Reveal.js` is a framework for easily creating beautiful presentations using
HTML.

`reveal.js` comes with a broad range of features including nested slides,
Markdown contents, PDF export, speaker notes and a JavaScript API. It's best
viewed in a modern browser but fallbacks are available to make sure your
presentation can still be viewed elsewhere.

## Dependencies

ocp-reveal depends on [dune], [omd], [js_of_ocaml]. You can install them using [opam]:

````sj
opam install dune js_of_ocaml omd
````````````

## Usage

This will build the library and the exemples.

```sh
dune build
```

After building, you can see the examples with:

```sh
xdg-open _build/default/examples/example.html
xdg-open _build/default/examples/example_2.html
```

You can also see an [online demo].

To start your own slides, you can have a look at the `examples` folder. It's just a matter of creating an HTML file, an OCaml file and compiling.

[dune]: https://dune.build/
[omd]: https://github.com/ocaml/omd
[js_of_ocaml]: https://github.com/ocsigen/js_of_ocaml
[online demo]: https://cagdas.bozman.fr/demo-ocp-reveal/#/
[opam]: https://opam.ocaml.org/
[reveal.js]: https://github.com/hakimel/reveal.js
