(* See https://github.com/ocaml/dune/issues/3214 *)

let init_predicates () =
  if Sys.ocaml_version >= "4.08.0" && Findlib.recorded_predicates () = [] then
    let preds = ["ppx_driver"; "mt"; "mt_posix"] in
    let preds = (if Dynlink.is_native then "native" else "byte") :: preds in
    Findlib.record_package_predicates preds

let load_pkg_if_needed ~debug pkg =
  try
    Fl_dynload.load_packages ~debug [pkg]
  with Dynlink.Error _ ->
    (* Module_already_loaded is not defined in OCaml <4.08.0 *)
    Findlib.record_package Findlib.Record_load pkg

(* The following function is adapted from findlib source code.
   (src/findlib/fl_dynload.ml)

Copyright 1999 by Gerd Stolpmann

The package "findlib" is copyright by Gerd Stolpmann.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this document and the "findlib" software (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

The Software is provided ``as is'', without warranty of any kind, express
or implied, including but not limited to the warranties of
merchantability, fitness for a particular purpose and noninfringement.
In no event shall Gerd Stolpmann be liable for any claim, damages or
other liability, whether in an action of contract, tort or otherwise,
arising from, out of or in connection with the Software or the use or
other dealings in the software.
*)

let load_packages ?(debug=false) pkgs =
  if Sys.ocaml_version < "4.08.0" then
    Fl_dynload.load_packages ~debug pkgs
  else
    let preds = Findlib.recorded_predicates() in
    let eff_pkglist =
      Findlib.package_deep_ancestors preds pkgs in
    List.iter (load_pkg_if_needed ~debug) eff_pkglist
