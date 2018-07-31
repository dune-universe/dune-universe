(* ppx_bigarray --- A PPX extension for providing big array literals in OCaml

   Copyright (C) 2015 Akinori ABE
   This software is distributed under MIT License
   See LICENSE.txt for details. *)

(** This is an auxiliary module for ppx_bigarray. *)

type ('a, 'b, 'c) alias =
  {
    kind : ('a, 'b) Bigarray.kind;
    layout : 'c Bigarray.layout;
  }

let is_c_layout : type a. a Bigarray.layout -> bool = function
  | Bigarray.C_layout -> true
  | Bigarray.Fortran_layout -> false
