## v0.10.4

- Fix an issue where cookies set from the command line sometimes
  disappeared

## v0.10.3

- Add a mechanism for ppx rewriters to pass informations to the build
  system

- Support `-as-ppx --cookie ...`

- Add `-corrected-suffix SUFFIX` to set the suffix for the corrected file

- Use the new generic ppx driver support of Jbuilder

- Use command line arguments registerd with ocaml-migrate-parsetree

## v0.10.2

- `-diff-cmd -` disables the diffing, to allow
  `[@@deriving_inline ...]` to work properly with jbuilder

## v0.10.1

- Disable checks by default

## 113.43.00

- Update for the new context free API

## 113.24.00

- Disable safety check when code transformations are used as standard
  "-ppx" rewriters.

- Introduce reserved namespaces, see `Ppx_core`'s changelog.

  Pass errors as attribute with -dparsetree to avoid
  "Error while running external preprocessor".

- Update to follow `Ppx_core` evolution.
