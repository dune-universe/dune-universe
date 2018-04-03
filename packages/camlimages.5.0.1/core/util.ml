(***********************************************************************)
(*                                                                     *)
(*                             CamlImages                              *)
(*                                                                     *)
(*            Fran輟is Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2014,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

external ( & ) : ('a -> 'b) -> 'a -> 'b = "%apply"

let (<<) = Bytes.set
let (<<!) = Bytes.unsafe_set

let range_check s from to_ =
  let len = Bytes.length s in
  if not (0 <= from && from <= to_ && to_ < len) then
    invalid_arg "index out of bounds"

let (>@!) = Array.unsafe_get

let (@%) s p = Char.code @@ Bytes.get s p

