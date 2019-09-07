# v0.2.2
* Removed `cartesian` word from definitions related to cartesian axes
* Functions for empty object construction are now prefixed with `empty_` instead of `create_`
* Minor type fixes
* Added helper functions like `of_string`, `of_bool` for some core types
* Added `chartjs_annotation.mli`
* Added some helper functions to `chartjs-annotation`
* Other naming changes
* Added rules for `ocp-indent` and `ocamlformat`. Format fixes
* Example fixes

# v0.2.1
* Added missing core chart options
* README.md updates
* Added functions for object construction for `datalabels` plugin
* Minor type fix in `datalabels` plugin
* Added bindings for `chartjs-plugin-colorschemes`
* Added bindings for `chartjs-plugin-annotation` 

# v0.2.0
* Migrated from `gen_js_api` to `js_of_ocaml` 

# v0.1.1
* Added `preservation` property to `chartjs-streaming` config object
* Added `make` function for `Options.Elements.Arc` object

# v0.1.0
Initial release.
* Added support for the majority of Chart.js options
* Added support for Line, Bar and Pie charts 
* Added bindings for `chartjs-plugin-streaming` and `chartjs-plugin-datalabels`
