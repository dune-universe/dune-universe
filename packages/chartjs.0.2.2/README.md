# chartjs-ocaml: OCaml bindings for Chart.js

This library provides OCaml bindings for the [Chart.js][chartjs] charting library 
and some popular plugins.

[chartjs]: https://github.com/chartjs/Chart.js

Following chart types are currently supported:
* Line
* Bar
* Horizontal bar
* Pie
* Doughnut

## Installation

### Opam
Install the library and its dependencies via [OPAM][opam]

[opam]: http://opam.ocaml.org

```bash
opam install chartjs
```

### Manual
```bash
dune build @install
dune install
```

## Usage

Please notice that `chartjs-ocaml` just provides bindings for the Chart.js
library and its most popular plugins, so be sure to include corresponding 
javascript files to your web application, like:

``` html
<script type="text/javascript" src="https://cdn.jsdelivr.net/npm/chart.js@2.8.0"></script>
```

Basic usage examples can be found in the [examples][examples] directory.

[examples]: https://github.com/monstasat/chartjs-ocaml/tree/master/examples

## Contents of the distribution

| Filename             | Description                                  |
|----------------------|----------------------------------------------|
| README               | this file                                    |
| LICENSE              | license and copyright notice                 |
| lib/                 | bindings for the Chart.js core library       |
| plugins/streaming    | bindings for the chartjs-plugin-streaming    |
| plugins/datalabels   | bindings for the chartjs-plugin-datalabels   |
| plugins/annotation   | bindings for the chartjs-plugin-annotation   |
| plugins/colorschemes | bindings for the chartjs-plugin-colorschemes |
| examples/            | basic library usage examples                 |
|                      |                                              |
