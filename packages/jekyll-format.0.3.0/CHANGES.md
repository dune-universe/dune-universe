v0.3.0 2021-08-05 Cambridge
---------------------------

- Shift to Yaml 3.0.0+ interface with separate `Yaml_sexp` module (@avsm #6)

v0.2.0 2021-04-25 Cambridge
---------------------------

- Add a `fields_to_yaml` function to convert the internal field data
  representation to a `Yaml.value` (#2 @patricoferris)
- `find` now returns a `Yaml.value option` rather than a `string option`
  (#1 @patricoferris)
- port to dune (@avsm)
- use Yaml library to parse front matter instead of a homebrew parser. (@avsm)
- use ocamlformat 0.18.0 (@avsm)
- minimum OCaml version supported is now 4.05+ (@avsm)

v0.1.0 2016-11-04 Cambridge
---------------------------

Initial public release.
