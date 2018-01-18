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
