## v0.4.2 - 2020-06-11

### Fixed
- Compatibility with OCaml 4.11. (Thanks to kit-ty-kate, closes: #8)

## v0.4.1 - 2019-10-02

### Fixed
- Improve documentation layout.
- Set minimum version of OCaml to 4.03.
- Add depext for Alpine Linux.

## v0.4.0 - 2019-10-02

### Changed
- Migrate build system to dune and opam.
- Make the package available on Linux, MacOSX and Windows.
- Reformat the documentation to use GitHub compatible Markdown.

### Fixed
- Move ocaml-xgettext from Camlp4 to ppx (thanks to Richard W.M. Jones).

## v0.3.8 - 2017-11-12
- Replace string by bytes, for compability with OCAml 4.06.

## v0.3.7 - 2017-03-01
- Add an extra step to generate configure.

## v0.3.6 - 2017-02-26
- Remove unix.cma linking argument for 4.0{3,4} compatibility

## v0.3.5 - 2014-08-04
- Always use format_of_string to not segfault with OCaml 4.02.

## v0.3.4 - 2011-08-09
- Use camomile 0.8.3

## v0.3.3 - 2009-11-01
- Upgrade ocaml-fileutils 0.4.0
- Improve gettext-stub C parts (memory management)
- Correctly handle OCaml string, wrt of escape char (Closes: FS#70)
- Remove the right file when uninstalling (Closes: FS#61)
- Enforce type safety for format string and plural (Closes: FS#72)
- Only output warning for install check that can be handle afterwards i
    (Closes: FS#74)
- Add a --strict flag to ocaml-gettext to make an error for every install check
    failed.
- Take into account Windows eol '\r' when parsing comments
    (Closes: FS#75)
- Don't include empty translation (Closes: FS#90)
- Check returned format string for fdgettext (Closes: FS#91)

## v0.3.2 - 2008-06-05
- Fix bug when buggy LANG/LC_ALL is set, using gettext stub (Richard W.M. Jones)

## v0.3.1 - 2008-05-09
- Fix bug when no LANG or LC_ALL is set
- Better autoconf use of @VERSION@
- Only remove build dir in top Makefile

## v0.3.0 - 2008-04-29
- Get rid of ocaml-ast-analyze (Richard W.M. Jones)
- Port of camlp4 extension to OCaml 3.10.1 (Richard W.M. Jones)
- Compile with camomile 0.7.1 (Richar W.M. Jones)
- Check dependency on ocaml-fileutils in configure
- Add --disable-doc-pdf to configure, to allow building doc without PDF (and
  fop)
- Get rid of camlidl
- Distribute .mli and .cmx file
- Handle multiline comment when merging, especially location "#:" and special
    comment "#,"
- Partially handle UTF-8, by not escaping string using "Printf.fprintf
    "%S"" but by using the same rule as the one used for reading escaped string
- Rework build system to:
  - use "install" programm to create dir and copy files
  - use --docdir and --prefix from configure
- Don't build PDF document by default
- Install documentation in $docdir/html/{api|api-ref|reference-manual}
- Fix typo in documentation

## v0.2.0
- Full rewrite of ocaml-gettext.
- Contains two version of the gettext engine : one is based on a purely OCaml
   approach (gettext-camomile), one is base on the former one (gettext-stub).
- The two engines shared a common interface based on gettext.base module.
- Most of the encoding/settings choice are accessible through command line
  option.
- Simplify the functions to accesse to gettext : 4 functions to translate
- Rebuild the string extractor with as a camlp4 analyzer
- Write test and benchmark program for maintaing the quality
- Write an example
- Write a manual
