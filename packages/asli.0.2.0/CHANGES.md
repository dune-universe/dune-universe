0.2.0 (2020-05-15)
------------------

* Handle more of Arm's specs (make grammar, etc. more flexible)
* Split libASL out from ASLi to make it easier to reuse
  parts of ASLi in other tools.
* Changed to semantic numbering system (i.e., 3-part format).
* Internal change to using Dune build system.
  This replaces the ocamlfind dependency with a dune dependency.


0.1 (2020-01-01)
----------------

* Added support for loading ELF files and executing binaries.


0.0 (2019-08-30)
----------------

Initial release of ASLi supporting

- loading ASL specifications
- evaluating expressions and statements
