(** Boolean variable on which a DD can branch. Morally just an integer, but
    modeled as a record for type safety. *)

open Base

module T = struct
  type t = Var of int
  [@@unboxed]
  [@@deriving compare, sexp, hash, eq]
end
include T

let leaf_idx = -1

let[@inline] inp (index : int) : t = Var (index * 2 + 1)
let[@inline] out (index : int) : t = Var (index * 2)

let[@inline] is_inp (Var id) : bool =
  id % 2 = 1

let[@inline] is_out (Var id) : bool =
  id % 2 = 0

let[@inline] to_out (Var id as var) : t =
  if is_out var then var else Var (id - 1)

let[@inline] is_in_out_pair (Var inp_id as inp) (Var out_id as out) : bool =
  is_inp inp && is_out out && (inp_id - 1 = out_id)

let[@inline] index (Var id) : int = id / 2

type closer_to_root = Left | Right | Equal

let[@inline] closer_to_root (Var id1) (Var id2) : closer_to_root =
  match Int.compare id1 id2 with
  | 0 -> Equal
  | _ when id1 > id2 -> Left
  | _ -> Right

let[@inline] idx_strictly_closer_to_root (idx0 : int) (idx1 : int) : bool =
  idx0 > idx1

let to_string (t : t) : string =
  Caml.Format.sprintf "%s%d"
    (if is_inp t then "I" else "O")
    (index t)
