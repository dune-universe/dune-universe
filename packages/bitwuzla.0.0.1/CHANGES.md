## 0.0.1 (2021-05-24)

- Fix compilation errors on non debian familly os:

    - Remove `bitwuzla-bin` package
      (it required non always available `static` version of system libraries)
    - Completely rework the building rules of foreign archives
      (side effect: drop `conf-cmake` dependency)
    - Fix Cadical compilation on `archlinux`

- Upgrade `dune` to 2.7 (fix opam lint warning)

## 0.0.0 (2021-05-20)

Initial release.

OCaml binding for the SMT solver [Bitwuzla](https://bitwuzla.github.io/).

- `Bitwuzla_c` library exposes low level functions from the
  [C API](https://bitwuzla.github.io/docs/c/interface.html);
- `Bitwuzla_z` library converts bitvector value to
  [Zarith](https://github.com/ocaml/Zarith) integer;
- `bitwuzla-bin` package installs Bitwuzla executable.

Vendor submodules:
- [Cadical](https://github.com/arminbiere/cadical) tag:sc2021
- [SymFPU](https://github.com/martin-cs/symfpu) commit:8fbe139
- [Btor2Tools](https://github.com/Boolector/btor2tools) commit:1df768d
- [Bitwuzla](https://github.com/bitwuzla/bitwuzla) commit:2f4dad6