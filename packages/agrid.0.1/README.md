# agrid [![Actions Status](https://github.com/ocamlpro/agrid/workflows/build/badge.svg)](https://github.com/ocamlpro/agrid/actions) [![coverage percentage](https://raw.githubusercontent.com/ocamlpro/agrid/gh-pages/coverage/badge.svg)](https://ocamlpro.github.io/agrid/coverage/)

<a href="https://www.deviantart.com/mademoiselleortie/art/Hagrid-666241515/"><img src="https://upload.wikimedia.org/wikipedia/commons/c/cd/Rubeus_Hagrid.jpg" alt="Fan art representing the character Rubeus Hagrid from the Harry Potter saga, made with charcoal and watercolours by Mademoiselle Ortie aka Elodie Tihange" width="200"/></a>

agrid is an [OCaml] library for adjustable grids. Adjustable grids are two dimensional arrays whose width/height can be changed by adding or removing row/column at either end (one at a time).

## Quickstart

You should depend on `agrid` then :

```ocaml
let () =
  let grid = Agrid.of_list [[1; 2]; [3; 4]] in
  let grid = Agrid.snoc_row grid (Flex_array.of_list [5; 6]) in
  Agrid.pp Format.pp_print_int Format.std_formatter grid
  (* prints:
   * 1; 2
   * 3; 4
   * 5; 6
   *)
```

For more, have a look at the [example] folder or at the [documentation].

## About

- [LICENSE]
- [CHANGELOG]

## Credits

The Hagrid drawing is licensed under the Creative Commons Attribution-Share Alike 4.0 International license. See more details on [wikimedia](https://commons.wikimedia.org/wiki/File:Rubeus_Hagrid.jpg).

[CHANGELOG]: ./CHANGES.md
[example]: ./example/
[LICENSE]: ./LICENSE.md

[documentation]: https://ocamlpro.github.io/agrid/
[OCaml]: https://ocaml.org
