(******************************************************************************)
(*                                                                            *)
(*                                    Sek                                     *)
(*                                                                            *)
(*          Arthur Charguéraud, Émilie Guermeur and François Pottier          *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

open Sek

module type CapacitySig =
  sig
    val capacity : int
  end


(* Stack implemented as a fixed-capacity array *)

module[@inline] Make
  (Capacity : CapacitySig)
  (OverwriteEmptySlots : OVERWRITE_EMPTY_SLOTS)
= struct

open Capacity
open OverwriteEmptySlots

(** Elements are stored in the array [data].
    The size of [data] is always equal to [capacity].
    The [size] field denotes the number of elements.
    It is such that [0 <= size <= capacity]. *)

type 'a t = {
  data : 'a array;
  mutable size : int;
  default : 'a; }

(** Auxiliary *)

let default s =
  s.default

let length s =
  s.size

let is_full s =
  length s = capacity

(** Stack operations *)

let create d =
  { data = Array.make capacity d;
    size = 0;
    default = d; }

let is_empty s =
  s.size = 0

let push x s =
  assert (s.size < capacity);
  let n = s.size in
  s.data.(n) <- x;
  s.size <- n + 1

let top s =
  if s.size = 0 then raise Not_found;
  s.data.(s.size-1)

let pop s =
  if s.size = 0 then raise Not_found;
  let new_n = s.size - 1 in
  let x = s.data.(new_n) in
  if overwrite_empty_slots
    then s.data.(new_n) <- s.default;
  s.size <- new_n;
  x

   (* Note that we could add:  *)

(** Random access *)

let get s i =
  assert (i >= 0 && i < s.size);
  s.data.(i)

let set s i v =
  assert (i >= 0 && i < s.size);
  s.data.(i) <- v

(** Iteration *)

let iter f q =
  for k = 0 to pred q.size do
    f q.data.(k);
  done

let fold_left f a q =
  let acc = ref a in
  for k = 0 to pred q.size do
    acc := f !acc q.data.(k);
  done;
  !acc

let fold_right f q a =
  let acc = ref a in
  for k = pred q.size downto 0 do
    acc := f q.data.(k) !acc;
  done;
  !acc

let to_list s =
  fold_left (fun a x -> x::a) [] s

(*
let to_list s =
  fold_right (fun x a -> x::a) s []
*)

end
